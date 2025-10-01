# Contributing to BZ-Data

Thank you for your interest in contributing to BZ-Data! This document provides guidelines and instructions for contributing.

## 🚀 Getting Started

1. **Fork the repository** on GitHub
2. **Clone your fork** locally:
   ```bash
   git clone https://github.com/YOUR_USERNAME/BZ-Data.git
   cd BZ-Data
   ```
3. **Install dependencies**:
   ```bash
   make install
   ```

## 🔧 Development Workflow

### Creating a Branch

Create a feature branch for your work:

```bash
git checkout -b feature/your-feature-name
```

Branch naming conventions:
- `feature/` - New features
- `fix/` - Bug fixes
- `docs/` - Documentation updates
- `refactor/` - Code refactoring
- `test/` - Adding or updating tests

### Making Changes

1. **Write clean, readable code** following R style guidelines
2. **Add tests** for new functionality in `tests/testthat/`
3. **Update documentation** as needed
4. **Test your changes**:
   ```bash
   make test
   make lint
   make preview  # For Quarto changes
   ```

### Commit Messages

Use conventional commit format:

```
type(scope): brief description

Longer description if needed

Fixes #123
```

Types:
- `feat`: New feature
- `fix`: Bug fix
- `docs`: Documentation changes
- `style`: Code style changes (formatting, etc.)
- `refactor`: Code refactoring
- `test`: Adding or updating tests
- `chore`: Maintenance tasks

Examples:
```
feat(plotting): add correlation heatmap function
fix(data-loader): handle missing date values
docs(readme): update installation instructions
```

### Testing

Before submitting:

```bash
# Run all tests
make test

# Lint R code
make lint

# Preview Quarto site
make preview

# Test Shiny app
make shiny
```

## 📝 Pull Request Process

1. **Update your branch** with latest main:
   ```bash
   git fetch upstream
   git rebase upstream/main
   ```

2. **Push your changes**:
   ```bash
   git push origin feature/your-feature-name
   ```

3. **Create a Pull Request** on GitHub with:
   - Clear title and description
   - Reference to related issues
   - Screenshots for UI changes
   - Test results

4. **Address review feedback** promptly

5. **Squash commits** if requested before merge

## 🎨 Code Style

### R Code

- Use `snake_case` for function and variable names
- Add roxygen2 documentation for functions
- Keep functions focused and modular
- Use tidyverse style guide

Example:
```r
#' Calculate summary statistics
#'
#' @param df Data frame
#' @param group_by Column name(s) to group by
#' @return Summary tibble
#' @export
calculate_summary <- function(df, group_by = "institution") {
  df %>%
    group_by(across(all_of(group_by))) %>%
    summarize(
      mean = mean(value, na.rm = TRUE),
      .groups = "drop"
    )
}
```

### Quarto/RMarkdown

- Use descriptive chunk labels
- Set appropriate chunk options
- Add comments for complex code
- Keep visualizations clear and labeled

## 🐛 Reporting Bugs

Create an issue with:

- **Clear title** describing the bug
- **Steps to reproduce** the issue
- **Expected behavior**
- **Actual behavior**
- **Environment details** (R version, OS, etc.)
- **Screenshots** if applicable

## 💡 Suggesting Features

Create an issue with:

- **Clear description** of the feature
- **Use case** and motivation
- **Proposed implementation** (if you have ideas)
- **Examples** from other projects (if relevant)

## 📚 Documentation

Help improve documentation:

- Fix typos and clarify instructions
- Add examples and use cases
- Update outdated information
- Improve code comments

## 🧪 Adding Tests

Tests are located in `tests/testthat/`. Add tests for:

- New functions in `R/`
- Bug fixes (regression tests)
- Edge cases and error handling

Example test:
```r
test_that("function_name handles edge case", {
  result <- function_name(edge_case_input)
  expect_equal(result, expected_output)
})
```

## 📦 Adding Dependencies

If adding new R packages:

1. Add to appropriate section in code
2. Update GitHub Actions workflows
3. Document in README
4. Justify the addition in PR description

## 🤝 Code of Conduct

- Be respectful and inclusive
- Welcome newcomers
- Focus on constructive feedback
- Assume good intentions

## 📞 Getting Help

- Open an issue for questions
- Tag maintainers for urgent matters
- Check existing issues and PRs first

## 🎉 Recognition

Contributors will be recognized in:
- GitHub contributors page
- Release notes for significant contributions
- README acknowledgments section

Thank you for contributing to BZ-Data! 🚀

