# SecCyL: Explorador de Secciones Censales de Castilla y León

SecCyL es una aplicación interactiva desarrollada en **Shiny** para identificar la sección censal más parecida a una sección de referencia en la comunidad de Castilla y León, utilizando la distancia de Mahalanobis robusta (estimada con MCD) sobre variables del Censo 2021 transformadas y estandarizadas.

## Características principales

- **Búsqueda instantánea** de la sección censal más similar.
- **Visualización en mapa** interactivo con Leaflet.
- **Selección dinámica** de provincia, municipio y sección.
- **Apartado específico** para análisis de capitales de provincia.
- **Mapas e indicadores**: renta, tasas de paro, empleo, actividad, etc.

## Requisitos

- R (versión >= 4.0)
- Paquetes R:
  - shiny
  - shinydashboard
  - leaflet
  - dplyr
  - readr
  - sf

## Instalación y uso

1. Clona este repositorio:
   ```bash
   git clone https://github.com/estreesantos/SecCyL-TFG.git
   ```
2. En R, instala los paquetes necesarios:
   ```r
   install.packages(c("shiny", "shinydashboard", "leaflet", "dplyr", "readr", "sf"))
   ```
3. Ejecuta la aplicación desde R o RStudio:
   ```r
   library(shiny)
   runApp("/ruta/al/proyecto/SecCyL-TFG/app")
   ```
   o abre `app.R` y pulsa **Run App**.

## Estructura de directorios

```
SecCyL-TFG/
├─ app.R            # Script principal de la app Shiny
├─ data/            # Datos de ejemplo (shapefiles, CSV)
├─ www/             # Recursos estáticos (logo, CSS)
├─ R/               # Funciones auxiliares (búsqueda Mahalanobis)
├─ README.md        # Descripción del proyecto
├─ .gitignore       # Archivos ignorados por Git
```

---

*Desarrollado por Estrella Santos Santos, TFG Grado en Estadística, Universidad de Valladolid (2025).*


