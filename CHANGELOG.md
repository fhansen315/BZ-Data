# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- Initial project structure with Quarto, RMarkdown, and Shiny
- Git LFS configuration for large data files
- Sample synthetic FX data (9 months, 3 institutions, 4 modalities)
- R utility functions for data loading, plotting, and analysis
- Enhanced Quarto site with interactive plotly visualizations
- FI Analysis page with institution-specific time series
- EDA page with modality-specific deep dives
- Data Dictionary page with schema and variable definitions
- Upgraded Shiny app with bslib theming and advanced features
- Custom SCSS theme for Quarto site
- GitHub Actions workflow for automated Quarto site deployment
- GitHub Actions workflow for R code checks
- Testing infrastructure with testthat
- Pre-commit hooks configuration
- Comprehensive README with badges and documentation
- CONTRIBUTING.md with development guidelines
- MIT License
- Makefile for common tasks automation

### Features

#### Data Layer
- `load_fx_data()` - Universal data loader supporting CSV and Excel
- `get_institutions()` - Extract unique institutions
- `get_modalities()` - Extract unique modalities
- `filter_by_institution()` - Filter data by institution
- `filter_by_modality()` - Filter data by modality
- `filter_by_date_range()` - Filter data by date range
- `validate_data()` - Data validation with schema checks

#### Visualization
- `plot_modality_timeseries()` - Interactive time series with plotly
- `plot_modality_histogram()` - Distribution histograms
- `plot_modality_boxplot()` - Comparative boxplots
- `plot_heatmap()` - Institution × Modality heatmap
- `summarize_data()` - Statistical summaries

#### Utilities
- `format_number()` - Number formatting with commas
- `format_currency()` - Currency formatting
- `pct_change()` - Percentage change calculation
- `moving_average()` - Moving average calculation

#### Quarto Site
- Home page with overview and quick stats
- FI Analysis page with:
  - Institution selector tabs
  - Time series by modality
  - Summary statistics tables
  - Distribution comparisons
  - Cross-institution heatmap
  - Trend analysis with moving averages
- EDA page with:
  - Overall distribution analysis
  - Density comparisons
  - Modality-specific tabs
  - Correlation matrices
  - Temporal pattern analysis
  - Interactive data table
- Data Dictionary page with:
  - Schema definitions
  - Variable descriptions
  - Data quality metrics
  - Summary statistics

#### Shiny Dashboard
- Three-tab interface (FI Analysis, EDA, Overview)
- Institution and modality selectors
- Date range filtering
- Interactive plotly charts
- Summary statistics tables
- Value boxes with key metrics
- Download buttons for filtered data
- Responsive bslib theming

#### CI/CD
- Automated Quarto site rendering and deployment to GitHub Pages
- R code linting and validation
- Test execution on push/PR

### Changed
- Enhanced README with comprehensive documentation
- Updated .gitignore for R, Python, Quarto, and Shiny artifacts
- Improved Quarto configuration with navigation and theming

### Fixed
- N/A (initial release)

### Security
- N/A

## [0.1.0] - 2025-09-30

### Added
- Initial project scaffold
- Basic Quarto site structure
- Simple Shiny app
- RMarkdown notebook template
- Data directory structure

---

## Release Notes

### Version 0.1.0 (Initial Release)

This is the initial release of BZ-Data, a comprehensive FX modality analytics platform.

**Highlights:**
- 🎨 Beautiful Quarto site with interactive visualizations
- 📊 Advanced Shiny dashboard with filtering and downloads
- 🔧 Reusable R utility functions
- 🤖 Automated CI/CD with GitHub Actions
- 📚 Comprehensive documentation
- ✅ Testing infrastructure

**Getting Started:**
```bash
git clone https://github.com/fhansen315/BZ-Data.git
cd BZ-Data
make install
make preview
```

**Contributors:**
- Chicão (@fhansen315)

---

[Unreleased]: https://github.com/fhansen315/BZ-Data/compare/v0.1.0...HEAD
[0.1.0]: https://github.com/fhansen315/BZ-Data/releases/tag/v0.1.0

