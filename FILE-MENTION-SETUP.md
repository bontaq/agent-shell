# File Mention Support - Setup Instructions

This guide explains how to use the file mention feature on any computer.

## What is File Mention Support?

File mention support allows you to attach files to your agent prompts using `@filename` syntax. The files are embedded inline with your message using the ACP `ContentBlock::Resource` type.

**Example:**
```
Compare @test1.txt with @test2.txt
```

This will embed both files' contents directly into the prompt sent to the agent.

## Installation

### 1. Clone or Update Repositories

You need both `acp.el` and `agent-shell` repositories with the file mention changes:

```bash
# Clone if you don't have them
cd ~/dev-repos  # or wherever you keep your repos
git clone https://github.com/xenodium/acp.el.git
git clone https://github.com/xenodium/agent-shell.git

# Or update existing repos
cd ~/dev-repos/acp.el
git pull

cd ~/dev-repos/agent-shell
git pull
```

### 2. Check Out the File Mention Branch

```bash
cd ~/dev-repos/agent-shell
git checkout feature/file-mention-support
```

### 3. Configure Doom Emacs (or your Emacs config)

Add to your `packages.el`:

```elisp
;; Use local development versions
(package! acp :recipe (:local-repo "~/dev-repos/acp.el"))
(package! agent-shell :recipe (:local-repo "~/dev-repos/agent-shell"))
```

**Note:** Adjust the paths if your repos are in a different location.

### 4. Sync Packages

```bash
doom sync
```

### 5. Restart Emacs

Restart Emacs to load the new versions.

## Usage

### Basic File Mentions

Type `@` followed by a filename:

```
Summarize @README.md
```

### Multiple Files

```
Compare @old-version.py with @new-version.py
```

### Files with Spaces

Use quotes:

```
Review @"my document.txt"
```

### Completion

- Type `@` and completion will automatically trigger (if using company-mode)
- Shows all files in your current project (uses `project.el` / git)
- Supports fuzzy matching (e.g., `@us.ai` matches `using-ai-notes.org`)
- Falls back to current directory if not in a git project

### Completion Patterns

- `@filename` - Project-wide completion (all files in git repo)
- `@path/file` - Files in relative subdirectory
- `@/abs/path` - Absolute path completion

## Features

### Auto-Completion
- Triggers immediately after typing `@`
- Fuzzy matching with `orderless` or built-in `flex` style
- Shows directories with `<dir>` annotation

### File Validation
- Checks file exists and is readable
- Validates file size (max 1MB by default)
- Checks if file is text (not binary)
- Verifies path is safe (within workspace)

### Error Handling
- If a file can't be read, shows error inline: `[Error reading file.txt: File not found]`
- Continues processing other files in the same message

### Attached Files Dialog
When files are successfully attached, you'll see a dialog showing:
```
2 files attached
  • test1.txt
  • test2.txt
```

## Configuration

### Adjust Max File Size

```elisp
;; Set max file size to 2MB
(setq agent-shell-file-mention-max-size (* 2 1024 1024))
```

### Add File Extensions

```elisp
;; Add custom text file extensions
(add-to-list 'agent-shell-file-mention-text-extensions "rkt")
(add-to-list 'agent-shell-file-mention-text-extensions "gleam")
```

## Troubleshooting

### Completion Doesn't Trigger

1. Check if company-mode is enabled: `M-x company-mode`
2. Verify you're in an agent-shell buffer
3. Try triggering manually: `M-x completion-at-point` after typing `@`

### Files Show as Errors

1. Check the file exists: `ls path/to/file`
2. Verify the agent-shell CWD: The buffer title shows `@ directory-name`
3. Try absolute paths: `@/full/path/to/file.txt`
4. Check file is text (not binary)
5. Verify file size is under 1MB

### No Project Files Shown

If `@` doesn't show project files:
1. Verify you're in a git repository: `git status`
2. Check `project.el` recognizes it: `M-x project-current`
3. Falls back to showing current directory files if no project found

## Technical Details

### How It Works

1. **Parsing**: Regex `agent-shell--file-mention-regex` finds all `@file` mentions
2. **Building**: `agent-shell--build-prompt-content` creates interleaved content blocks:
   - Text blocks for normal text
   - Resource blocks for file contents
3. **Protocol**: Uses ACP `ContentBlock::Resource` with `embeddedContext` capability
4. **CWD**: Files are resolved relative to agent-shell's working directory (shown in buffer title)

### Content Block Structure

For input: `"Compare @test1.txt with @test2.txt"`

Creates 4 blocks:
1. Text: `"Compare "`
2. Resource: test1.txt contents
3. Text: `" with "`
4. Resource: test2.txt contents

### Files Modified

**acp.el:**
- Added `:embedded-context-capability` parameter to `acp-make-initialize-request`

**agent-shell.el:**
- Added file mention regex and parsing (lines 692-767)
- Added completion-at-point integration (lines 174-234)
- Added prompt builder with resource blocks (lines 769-848)
- Added validation helpers (lines 716-744)
- Enabled capability in initialization (line 321)

## Git Branches

- **acp.el**: Should be on `main` (embedded-context support merged)
- **agent-shell**: Use branch `feature/file-mention-support`

## Verifying Installation

1. Start an agent shell: `M-x agent-shell-anthropic-start-claude-code`
2. Type `@` - should see completion popup
3. Send a message like: `Summarize @README.md`
4. Check for "1 file attached" dialog
5. Enable logging to verify: `M-x agent-shell-toggle-logging`
6. View traffic: `M-x agent-shell-view-traffic`
7. Look for `ContentBlock::Resource` entries with `type: "resource"`

## Support

If you encounter issues:
1. Check `*Messages*` buffer for errors
2. Enable logging and check traffic buffer
3. Verify local repo paths in packages.el
4. Ensure you're on the correct git branch
