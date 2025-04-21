'''Mi script de funciones auxiliares'''

# Librerias
import pandas as pd
import numpy as np

# Revision de columnas
def univar_analisis(data, flag_var=False):

    """
    Realiza un análisis univariado de las columnas en un DataFrame, calculando estadísticas descriptivas y la presencia de valores faltantes o infinitos.

    Args:
        - **data** (`pd.DataFrame`): DataFrame de entrada con las variables a analizar.
        - **flag_var** (`bool`, opcional): Si es `True`, imprime un mensaje con el análisis para cada variable procesada. Por defecto, `False`.

    Return:
        - Devuelve un DataFrame con las siguientes columnas:
            - **Variable**: Nombre de la columna.
            - **DataType**: Tipo de dato de la columna.
            - **Total**: Número total de registros en la columna.
            - **No Missings**: Número de valores no faltantes.
            - **Missings**: Número de valores faltantes (`NaN`).
            - **% Missings**: Porcentaje de valores faltantes respecto al total.
            - **Inf Count**: Número de valores infinitos positivos.
            - **-Inf Count**: Número de valores infinitos negativos.
            - **Promedio**: Media aritmética (solo para columnas numéricas).
            - **Desviación Estandar**: Desviación estándar (solo para columnas numéricas).
            - **Varianza**: Varianza (solo para columnas numéricas).
            - **Mínimo**: Valor mínimo (solo para columnas numéricas).
            - **p1**: Percentil 1 (solo para columnas numéricas).
            - **p5**: Percentil 5 (solo para columnas numéricas).
            - **p10**: Percentil 10 (solo para columnas numéricas).
            - **q1**: Primer cuartil (percentil 25) (solo para columnas numéricas).
            - **Mediana**: Valor mediano (percentil 50) (solo para columnas numéricas).
            - **q3**: Tercer cuartil (percentil 75) (solo para columnas numéricas).
            - **p90**: Percentil 90 (solo para columnas numéricas).
            - **p95**: Percentil 95 (solo para columnas numéricas).
            - **p99**: Percentil 99 (solo para columnas numéricas).
            - **Máximo**: Valor máximo (solo para columnas numéricas).

    Notes:
        - Si una columna no es numérica o es booleana, los valores estadísticos se registran como `None`.
        - Las columnas categóricas no son procesadas para estadísticas numéricas.
        - Valores infinitos (`inf` y `-inf`) son contabilizados y diferenciados.
        - El análisis considera valores faltantes (`NaN`) y nulos (`isnull`).
    """

    results = []

    for feature in data.columns:
        data_type = data[feature].dtype  

        total = data.shape[0]
        nan_count = data[feature].isna().sum()
        no_missings = total - nan_count
        pct_missings = nan_count / total

        if pd.api.types.is_numeric_dtype(data[feature]) and not pd.api.types.is_bool_dtype(data[feature]):  
            promedio = data[feature].mean()
            desv_estandar = data[feature].std()
            varianza = data[feature].var()
            minimo = data[feature].min()
            p1 = data[feature].quantile(0.01)
            p5 = data[feature].quantile(0.05)
            p10 = data[feature].quantile(0.1)
            q1 = data[feature].quantile(0.25)
            mediana = data[feature].quantile(0.5)
            q3 = data[feature].quantile(0.75)
            p90 = data[feature].quantile(0.9)
            p95 = data[feature].quantile(0.95)
            p99 = data[feature].quantile(0.99)
            maximo = data[feature].max()

            inf_count = np.isinf(data[feature]).sum()
            neg_inf_count = (data[feature] == -np.inf).sum()
        else:
           
            promedio = None
            desv_estandar = None
            varianza = None
            minimo = None
            p1 = None
            p5 = None
            p10 = None
            q1 = None
            mediana = None
            q3 = None
            p90 = None
            p95 = None
            p99 = None
            maximo = None
            inf_count = 0
            neg_inf_count = 0

        results.append({
            'Variable': feature,
            'DataType':data_type,
            'Total': total,
            'No Missings': no_missings,
            'Missings': nan_count,
            '% Missings': pct_missings,
            'Inf Count': inf_count,
            '-Inf Count': neg_inf_count,
            'Promedio': promedio,
            'Desviación Estandar': desv_estandar,
            'Varianza': varianza,
            'Mínimo': minimo,
            'p1': p1,
            'p5': p5,
            'p10': p10,
            'q1': q1,
            'Mediana': mediana,
            'q3': q3,
            'p90': p90,
            'p95': p95,
            'p99': p99,
            'Máximo': maximo
        })

        if flag_var:
            print(f"Análisis para {feature}:")

    return pd.DataFrame(results)

# Estandarizar nombres de columnas
def std_nombres_columnas(data):
    """
    Estandariza el nombre de las columnas, rempplazando espacios por underscore y usando minusculas en los nombres

    Args:
        data (pd.DataFrame): El DataFrame a procesar.

    Returns:
        el nombre de las columnas estandarizadas
    """
    # Convertir nombres de las columnas a minúsculas y reemplazar espacios por guiones bajos
    data.rename(columns=lambda x: x.lower().replace(" ", "_"), inplace=True)
    
    # Devolver los nombres de las columnas modificadas
    return data.columns
