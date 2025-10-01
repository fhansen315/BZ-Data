# Adding GitHub Actions Workflows

The GitHub Actions workflow files are in `.github/workflows/` but couldn't be pushed automatically due to OAuth scope limitations.

## Option 1: Push with Personal Access Token (Recommended)

1. **Create a Personal Access Token** with `workflow` scope:
   - Go to: https://github.com/settings/tokens/new
   - Name: "BZ-Data Workflows"
   - Expiration: 30 days (or your preference)
   - Select scopes: ✅ `workflow` (and `repo` if not already selected)
   - Click "Generate token"
   - **Copy the token** (you won't see it again!)

2. **Push using the token**:
   ```bash
   # Add the workflow files
   git add .github/workflows/
   git commit -m "ci: add GitHub Actions workflows for Quarto publish and R checks"
   
   # Push with token (replace YOUR_TOKEN)
   git push https://YOUR_TOKEN@github.com/fhansen315/BZ-Data.git main
   ```

3. **Clean up** (optional):
   ```bash
   # Remove token from git history
   git remote set-url origin https://github.com/fhansen315/BZ-Data.git
   ```

## Option 2: Add Workflows via GitHub Web Interface

1. **Go to your repo**: https://github.com/fhansen315/BZ-Data

2. **Create first workflow**:
   - Click "Actions" tab
   - Click "New workflow"
   - Click "set up a workflow yourself"
   - Name: `quarto-publish.yml`
   - Copy content from `.github/workflows/quarto-publish.yml`
   - Commit directly to main

3. **Create second workflow**:
   - Click "Actions" tab
   - Click "New workflow"
   - Click "set up a workflow yourself"
   - Name: `r-check.yml`
   - Copy content from `.github/workflows/r-check.yml`
   - Commit directly to main

4. **Pull the changes locally**:
   ```bash
   git pull origin main
   ```

## Option 3: Use GitHub CLI

If you have GitHub CLI installed:

```bash
# Login with workflow scope
gh auth login --scopes workflow

# Add and push
git add .github/workflows/
git commit -m "ci: add GitHub Actions workflows for Quarto publish and R checks"
git push origin main
```

## What These Workflows Do

### `quarto-publish.yml`
- **Triggers**: On push to main, PRs, or manual dispatch
- **Actions**:
  - Installs R and Quarto
  - Installs R dependencies
  - Renders the Quarto site
  - Deploys to GitHub Pages

### `r-check.yml`
- **Triggers**: On push/PR to main/develop with R file changes
- **Actions**:
  - Installs R and dependencies
  - Runs lintr on R code
  - Checks R scripts for syntax errors

## Enable GitHub Pages

After workflows are added:

1. Go to: https://github.com/fhansen315/BZ-Data/settings/pages
2. Source: **GitHub Actions**
3. Save

Your site will be live at: `https://fhansen315.github.io/BZ-Data/`

## Verify Workflows

After adding:

1. Go to: https://github.com/fhansen315/BZ-Data/actions
2. You should see workflow runs
3. Check for green checkmarks ✅

## Troubleshooting

### Workflow fails with "Pages not enabled"
- Enable GitHub Pages in Settings → Pages → Source: GitHub Actions

### Workflow fails with "Permission denied"
- Go to Settings → Actions → General
- Workflow permissions: Select "Read and write permissions"
- Save

### R dependencies fail to install
- Check the workflow logs
- May need to add system dependencies to workflow file

## Current Status

✅ All code pushed to GitHub (except workflows)  
⏳ Workflows ready locally in `.github/workflows/`  
📋 Follow one of the options above to add them  

---

**Need help?** Open an issue or check the GitHub Actions documentation.

