import streamlit as st
import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
import plotly.figure_factory as ff
from plotly.subplots import make_subplots
import numpy as np
from scipy import stats
import uuid
import json
from typing import Dict, List, Optional, Tuple

# Load mtcars dataset (simulate R's built-in dataset)
@st.cache_data
def load_mtcars():
    # Using a dictionary to mimic mtcars; alternatively, load from CSV
    mtcars = pd.read_csv("https://raw.githubusercontent.com/plotly/datasets/master/mtcars.csv")
    # Convert relevant columns to categorical (factor equivalent)
    mtcars['cyl'] = mtcars['cyl'].astype('category')
    mtcars['vs'] = mtcars['vs'].astype('category')
    mtcars['am'] = mtcars['am'].map({0: 'Automatic', 1: 'Manual'}).astype('category')
    mtcars['gear'] = mtcars['gear'].astype('category')
    mtcars['carb'] = mtcars['carb'].astype('category')
    return mtcars

# Helper function to generate UI for a single layer
def layer_controls_ui(layer_id: str, df: pd.DataFrame, layer_index: int) -> Dict:
    st.subheader(f"Layer {layer_index + 1}")
    
    with st.expander("Aesthetics", expanded=True):
        geom_type = st.selectbox(
            f"Geometric Object (Layer {layer_index + 1})",
            options=[
                'scatter', 'line', 'bar', 'histogram', 'box', 'violin', 'area',
                'density_2d', 'hexbin', 'smooth', 'hline', 'vline', 'abline'
            ],
            key=f"geom_type_{layer_id}"
        )
        
        x_var = st.selectbox(
            f"X Variable (Layer {layer_index + 1})",
            options=['None'] + list(df.columns),
            key=f"x_var_{layer_id}"
        )
        
        y_var = None
        if geom_type in ['scatter', 'line', 'bar', 'area', 'smooth', 'box', 'violin', 'density_2d', 'hexbin']:
            y_var = st.selectbox(
                f"Y Variable (Layer {layer_index + 1})",
                options=['None'] + list(df.columns),
                key=f"y_var_{layer_id}"
            )
        
        color_var = st.selectbox(
            f"Color Variable (Layer {layer_index + 1})",
            options=['None'] + list(df.columns),
            key=f"color_var_{layer_id}"
        )
        
        fill_var = st.selectbox(
            f"Fill Variable (Layer {layer_index + 1})",
            options=['None'] + list(df.columns),
            key=f"fill_var_{layer_id}"
        )
        
        group_var = st.selectbox(
            f"Group Variable (Layer {layer_index + 1})",
            options=['None'] + list(df.columns),
            key=f"group_var_{layer_id}"
        )
        
        size_var = st.selectbox(
            f"Size Variable (Layer {layer_index + 1})",
            options=['None'] + list(df.columns.select_dtypes(include=np.number)),
            key=f"size_var_{layer_id}"
        )
        
        if geom_type in ['scatter', 'line', 'smooth', 'box', 'violin']:
            fixed_size = st.number_input(
                f"Fixed Size (Layer {layer_index + 1})",
                min_value=0.1, value=1.0, step=0.1,
                key=f"size_val_{layer_id}"
            )
        else:
            fixed_size = None
        
        alpha_var = st.selectbox(
            f"Alpha Variable (Layer {layer_index + 1})",
            options=['None'] + list(df.columns.select_dtypes(include=np.number)),
            key=f"alpha_var_{layer_id}"
        )
        
        fixed_alpha = st.number_input(
            f"Fixed Alpha (0-1) (Layer {layer_index + 1})",
            min_value=0.0, max_value=1.0, value=0.8, step=0.1,
            key=f"alpha_val_{layer_id}"
        )
        
        if geom_type == 'scatter':
            shape_var = st.selectbox(
                f"Shape Variable (Layer {layer_index + 1})",
                options=['None'] + list(df.columns.select_dtypes(include='category')),
                key=f"shape_var_{layer_id}"
            )
        else:
            shape_var = None
        
        if geom_type in ['line', 'smooth', 'hline', 'vline', 'abline']:
            linetype = st.selectbox(
                f"Linetype (Layer {layer_index + 1})",
                options=['solid', 'dash', 'dot', 'dashdot'],
                key=f"linetype_val_{layer_id}"
            )
        else:
            linetype = None
    
    with st.expander("Axis & Scale", expanded=False):
        x_trans = st.selectbox(
            f"X-axis Transformation (Layer {layer_index + 1})",
            options=['none', 'log2', 'log10', 'sqrt', 'reverse'],
            key=f"x_trans_{layer_id}"
        )
        x_min = st.number_input(f"X-axis Minimum (Layer {layer_index + 1})", value=None, key=f"xmin_{layer_id}")
        x_max = st.number_input(f"X-axis Maximum (Layer {layer_index + 1})", value=None, key=f"xmax_{layer_id}")
        
        y_trans = st.selectbox(
            f"Y-axis Transformation (Layer {layer_index + 1})",
            options=['none', 'log2', 'log10', 'sqrt', 'reverse'],
            key=f"y_trans_{layer_id}"
        )
        y_min = st.number_input(f"Y-axis Minimum (Layer {layer_index + 1})", value=None, key=f"ymin_{layer_id}")
        y_max = st.number_input(f"Y-axis Maximum (Layer {layer_index + 1})", value=None, key=f"ymax_{layer_id}")
    
    with st.expander("Labels & Title", expanded=False):
        title = st.text_input(f"Title (Layer {layer_index + 1})", key=f"title_{layer_id}")
        subtitle = st.text_input(f"Subtitle (Layer {layer_index + 1})", key=f"subtitle_{layer_id}")
        caption = st.text_input(f"Caption (Layer {layer_index + 1})", key=f"caption_{layer_id}")
        x_label = st.text_input(f"X-axis Label (Layer {layer_index + 1})", value=x_var if x_var != 'None' else '', key=f"x_label_{layer_id}")
        y_label = st.text_input(f"Y-axis Label (Layer {layer_index + 1})", value=y_var if y_var != 'None' else '', key=f"y_label_{layer_id}")
    
    # Layer-specific parameters
    abline_params = hline_params = vline_params = None
    if geom_type == 'abline':
        abline_params = {
            'intercept': st.number_input(f"Intercept (Layer {layer_index + 1})", value=0.0, key=f"abline_intercept_{layer_id}"),
            'slope': st.number_input(f"Slope (Layer {layer_index + 1})", value=1.0, key=f"abline_slope_{layer_id}")
        }
    elif geom_type == 'hline':
        hline_params = {
            'yintercept': st.number_input(f"Y-intercept (Layer {layer_index + 1})", value=0.0, key=f"hline_yintercept_{layer_id}")
        }
    elif geom_type == 'vline':
        vline_params = {
            'xintercept': st.number_input(f"X-intercept (Layer {layer_index + 1})", value=0.0, key=f"vline_xintercept_{layer_id}")
        }
    
    return {
        'geom_type': geom_type,
        'x_var': x_var,
        'y_var': y_var,
        'color_var': color_var,
        'fill_var': fill_var,
        'group_var': group_var,
        'size_var': size_var,
        'fixed_size': fixed_size,
        'alpha_var': alpha_var,
        'fixed_alpha': fixed_alpha,
        'shape_var': shape_var,
        'linetype': linetype,
        'x_trans': x_trans,
        'x_min': x_min,
        'x_max': x_max,
        'y_trans': y_trans,
        'y_min': y_min,
        'y_max': y_max,
        'title': title,
        'subtitle': subtitle,
        'caption': caption,
        'x_label': x_label,
        'y_label': y_label,
        'abline_params': abline_params,
        'hline_params': hline_params,
        'vline_params': vline_params
    }

# Helper function to generate a Plotly trace for a layer
def generate_plotly_trace(layer_config: Dict, df: pd.DataFrame) -> Optional[go.Trace]:
    geom_type = layer_config['geom_type']
    x_var = layer_config['x_var']
    y_var = layer_config['y_var']
    color_var = layer_config['color_var']
    fill_var = layer_config['fill_var']
    group_var = layer_config['group_var']
    size_var = layer_config['size_var']
    fixed_size = layer_config['fixed_size']
    fixed_alpha = layer_config['fixed_alpha']
    shape_var = layer_config['shape_var']
    linetype = layer_config['linetype']
    
    if x_var == 'None' or (geom_type not in ['hline', 'vline', 'abline'] and y_var == 'None'):
        return None
    
    try:
        if geom_type == 'scatter':
            trace = go.Scatter(
                x=df[x_var] if x_var != 'None' else None,
                y=df[y_var] if y_var != 'None' else None,
                mode='markers',
                marker=dict(
                    size=df[size_var] * 10 if size_var != 'None' else fixed_size * 10 if fixed_size else 10,
                    opacity=df[fixed_alpha] if fixed_alpha else 0.8,
                    color=df[color_var] if color_var != 'None' else None,
                ),
                name=f"Layer {geom_type}",
                showlegend=True
            )
        elif geom_type == 'line':
            trace = go.Scatter(
                x=df[x_var] if x_var != 'None' else None,
                y=df[y_var] if y_var != 'None' else None,
                mode='lines',
                line=dict(
                    dash=linetype,
                    color=df[color_var].iloc[0] if color_var != 'None' else None
                ),
                opacity=fixed_alpha if fixed_alpha else 0.8,
                name=f"Layer {geom_type}",
                showlegend=True
            )
        elif geom_type == 'bar':
            trace = go.Bar(
                x=df[x_var] if x_var != 'None' else None,
                y=df[y_var] if y_var != 'None' else None,
                marker=dict(
                    color=df[color_var] if color_var != 'None' else None,
                    opacity=fixed_alpha if fixed_alpha else 0.8
                ),
                name=f"Layer {geom_type}",
                showlegend=True
            )
        elif geom_type == 'histogram':
            trace = go.Histogram(
                x=df[x_var] if x_var != 'None' else None,
                marker=dict(
                    color=df[color_var] if color_var != 'None' else None,
                    opacity=fixed_alpha if fixed_alpha else 0.8
                ),
                name=f"Layer {geom_type}",
                showlegend=True
            )
        elif geom_type == 'box':
            trace = go.Box(
                x=df[x_var] if x_var != 'None' else None,
                y=df[y_var] if y_var != 'None' else None,
                marker=dict(
                    color=df[color_var] if color_var != 'None' else None,
                    opacity=fixed_alpha if fixed_alpha else 0.8
                ),
                name=f"Layer {geom_type}",
                showlegend=True
            )
        elif geom_type == 'violin':
            trace = go.Violin(
                x=df[x_var] if x_var != 'None' else None,
                y=df[y_var] if y_var != 'None' else None,
                marker=dict(
                    color=df[color_var] if color_var != 'None' else None,
                    opacity=fixed_alpha if fixed_alpha else 0.8
                ),
                name=f"Layer {geom_type}",
                showlegend=True
            )
        elif geom_type == 'hline':
            trace = go.Scatter(
                x=[df[x_var].min(), df[x_var].max()] if x_var != 'None' else [0, 1],
                y=[layer_config['hline_params']['yintercept']] * 2,
                mode='lines',
                line=dict(dash=linetype, color='black'),
                name=f"Layer {geom_type}",
                showlegend=True
            )
        elif geom_type == 'vline':
            trace = go.Scatter(
                x=[layer_config['vline_params']['xintercept']] * 2,
                y=[df[y_var].min(), df[y_var].max()] if y_var != 'None' else [0, 1],
                mode='lines',
                line=dict(dash=linetype, color='black'),
                name=f"Layer {geom_type}",
                showlegend=True
            )
        elif geom_type == 'abline':
            x_range = [df[x_var].min(), df[x_var].max()] if x_var != 'None' else [0, 1]
            y_vals = [
                layer_config['abline_params']['intercept'] + layer_config['abline_params']['slope'] * x
                for x in x_range
            ]
            trace = go.Scatter(
                x=x_range,
                y=y_vals,
                mode='lines',
                line=dict(dash=linetype, color='black'),
                name=f"Layer {geom_type}",
                showlegend=True
            )
        else:
            return None
        return trace
    except Exception as e:
        st.warning(f"Error in layer {geom_type}: {str(e)}")
        return None

# Main app
def main():
    st.set_page_config(page_title="Interactive Plotting App", layout="wide")
    st.title("mtcars Interactive Plotting")
    
    df = load_mtcars()
    
    # Initialize session state for layers
    if 'layers' not in st.session_state:
        st.session_state.layers = [{'id': str(uuid.uuid4()), 'index': 0}]
    
    # Sidebar for controls
    with st.sidebar:
        tabs = st.tabs(["Layers", "Faceting", "Statistics", "Theme"])
        
        with tabs[0]:
            # Layer management
            st.subheader("Manage Layers")
            col1, col2 = st.columns(2)
            with col1:
                if st.button("Add Layer"):
                    new_layer = {'id': str(uuid.uuid4()), 'index': len(st.session_state.layers)}
                    st.session_state.layers.append(new_layer)
            with col2:
                if st.button("Remove Last Layer") and len(st.session_state.layers) > 1:
                    st.session_state.layers.pop()
            
            # Render layer controls
            layer_configs = []
            for layer in st.session_state.layers:
                with st.expander(f"Layer {layer['index'] + 1} Settings", expanded=True):
                    layer_config = layer_controls_ui(layer['id'], df, layer['index'])
                    layer_configs.append(layer_config)
        
        with tabs[1]:
            st.subheader("Faceting")
            facet_row = st.selectbox("Facet by Row", options=['None'] + list(df.select_dtypes(include='category').columns))
            facet_col = st.selectbox("Facet by Column", options=['None'] + list(df.select_dtypes(include='category').columns))
            facet_scales = st.selectbox("Facet Scales", options=['fixed', 'free_x', 'free_y', 'free'], key='facet_scales')
        
        with tabs[2]:
            st.subheader("Statistics")
            add_stat_summary = st.checkbox("Add Statistical Summary")
            if add_stat_summary:
                stat_summary_fun = st.selectbox("Summary Function", options=['mean', 'median'])
                stat_summary_geom = st.selectbox("Summary Geom", options=['point', 'errorbar'])
                stat_summary_width = st.number_input("Summary Width", min_value=0.1, value=0.5, step=0.1)
            
            add_stat_tests = st.checkbox("Add Statistical Tests (Box/Violin)")
            stat_test_type = None
            stat_test_alpha = None
            if add_stat_tests and layer_configs and layer_configs[0]['geom_type'] in ['box', 'violin']:
                stat_test_type = st.selectbox("Test Type", options=['t-test', 'Wald test'])
                stat_test_alpha = st.number_input("Significance Level (alpha)", min_value=0.01, max_value=0.5, value=0.05, step=0.01)
        
        with tabs[3]:
            st.subheader("Theme")
            theme = st.selectbox("Theme", options=['plotly', 'plotly_white', 'plotly_dark', 'ggplot2', 'seaborn'])
            customize_theme = st.checkbox("Customize Theme")
            theme_params = {}
            if customize_theme:
                theme_params['text_size'] = st.number_input("Text Size", value=12, min_value=8)
                theme_params['text_color'] = st.color_picker("Text Color", value="#000000")
                theme_params['bg_color'] = st.color_picker("Background Color", value="#FFFFFF")
                theme_params['grid_color'] = st.color_picker("Grid Color", value="#D3D3D3")
                theme_params['legend_position'] = st.selectbox("Legend Position", options=['top', 'bottom', 'left', 'right', 'none'])
    
    # Main panel
    st.button("Reset to Default", on_click=lambda: st.session_state.update({'layers': [{'id': str(uuid.uuid4()), 'index': 0}]}))
    
    # Generate Plotly figure
    fig = go.Figure()
    for layer_config in layer_configs:
        trace = generate_plotly_trace(layer_config, df)
        if trace:
            fig.add_trace(trace)
    
    # Apply faceting
    if facet_row != 'None' or facet_col != 'None':
        facet_spec = {}
        if facet_row != 'None':
            facet_spec['row'] = facet_row
        if facet_col != 'None':
            facet_spec['col'] = facet_col
        fig = px.scatter(df, x=layer_configs[0]['x_var'], y=layer_configs[0]['y_var'], facet_row=facet_row if facet_row != 'None' else None, facet_col=facet_col if facet_col != 'None' else None)
    
    # Apply theme
    fig.update_layout(
        template=theme,
        title=layer_configs[0]['title'] if layer_configs[0]['title'] else None,
        xaxis_title=layer_configs[0]['x_label'],
        yaxis_title=layer_configs[0]['y_label'],
        showlegend=True,
        plot_bgcolor=theme_params.get('bg_color', '#FFFFFF'),
        font=dict(size=theme_params.get('text_size', 12), color=theme_params.get('text_color', '#000000')),
        legend=dict(orientation='h' if theme_params.get('legend_position') == 'top' else 'v')
    )
    
    # Statistical tests
    if add_stat_tests and stat_test_type and layer_configs and layer_configs[0]['geom_type'] in ['box', 'violin']:
        x_var = layer_configs[0]['x_var']
        y_var = layer_configs[0]['y_var']
        if x_var != 'None' and y_var != 'None' and df[x_var].nunique() == 2:
            group1 = df[df[x_var] == df[x_var].cat.categories[0]][y_var]
            group2 = df[df[x_var] == df[x_var].cat.categories[1]][y_var]
            if stat_test_type == 't-test':
                stat_result = stats.ttest_ind(group1, group2)
                st.write(f"t-test: Statistic={stat_result.statistic:.3f}, p-value={stat_result.pvalue:.4f}")
            elif stat_test_type == 'Wald test':
                mean_diff = group1.mean() - group2.mean()
                se = np.sqrt(group1.var()/len(group1) + group2.var()/len(group2))
                wald_stat = mean_diff / se
                p_value = 2 * (1 - stats.norm.cdf(abs(wald_stat)))
                st.write(f"Wald test: Statistic={wald_stat:.3f}, p-value={p_value:.4f}")
    
    st.plotly_chart(fig, use_container_width=True)

if __name__ == "__main__":
    main()
