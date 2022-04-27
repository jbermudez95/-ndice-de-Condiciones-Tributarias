# Indice_de_Condiciones_Tributarias
Este repositorio contiene el código de R y Stata con los que se generan las estimaciones del trabajo "Estimación de un Índice de Condiciones Tributarias para Honduras", DT 01-2021, Servicio de Administración de Rentas. Por Jose Carlo Bermúdez. Véase https://www.sar.gob.hn/download/estimacion-de-un-indice-de-condiciones-tributarias-para-honduras/

El código en R "isre.R" utiliza una base de datos con series de tiempo para un vector de variables que luego son reducidas utilizando Análisis de Componentes Principales y se construye el Índice de Condiciones Tributarias. Este código produce todas las gráficas presentadas dentro del paper y genera una base de datos que es exportada a Stata para realizar ejercicios de pronóstico.

El código en Stata "forecasting_ict_test.do" utiliza una base de datos generada por el código de R y deesarolla una serie de ejercicios de pronósticos dinámicos fuera de muestra para, finalmente, aplicar la prueba de capacidad predictiva de Diebold y Mariano.
