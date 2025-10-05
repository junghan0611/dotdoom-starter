# Terminal & CJK Optimization Guide

## Problem Statement

When using Emacs in terminal mode with CJK (Chinese, Japanese, Korean) languages, character width inconsistencies can break layout and alignment:

1. **ASCII Art Banners**: Large ASCII banners cause layout issues in split windows
2. **Emoji Width**: Emoji characters don't always render with consistent width
3. **CJK Character Width**: Mixed English/CJK text can misalign in terminal emulators
4. **Window Splitting**: Narrow windows become unusable with wide splash screens

### Why This Matters

- **Western Users**: Typically use English-only, rarely encounter width issues
- **CJK Users**: Face daily layout breaks when mixing languages and symbols
- **Terminal Users**: More sensitive to character width problems than GUI users

This configuration prioritizes **reliable terminal experience** over visual flair.

## Our Solution

### 1. Minimal Dashboard (Terminal-First)

**Removed**: Large ASCII art banners that break in split windows

**Added**: Simple text fortune widget
- Uses `fortune` command if available
- Falls back to Kevin Kelly wisdom quote
- Single line, clean, never breaks layout

```elisp
;; config.el:105-136
(setq +doom-dashboard-ascii-banner-fn nil)  ; No ASCII art

(defun my/dashboard-widget-fortune ()
  "Display fortune quote or Kevin Kelly's wisdom."
  (let ((quote
         (if (executable-find "fortune")
             (shell-command-to-string "fortune -c 90% advice 10% .")
           "\n The only way to fight against getting old is to remain astonished.\n                                                      — Kevin Kelly")))
    (+doom-dashboard--center
     (- +doom-dashboard--width 2)
     (insert quote "\n"))))
```

### 2. Font Width Normalization

**CJK Character Handling** (config.el:196-252):

```elisp
(defun my/set-emoji-symbol-font ()
  "Set consistent emoji and symbol fonts for terminal/GUI."
  ;; Hangul uses default monospace family
  (set-fontset-font "fontset-default" 'hangul
    (font-spec :family (face-attribute 'default :family)))

  (unless (display-graphic-p) ; terminal-specific
    ;; Force emoji width = 2 (double-width)
    (dolist (range '((#x1F300 . #x1F6FF)  ; Emoji ranges
                     (#x2600 . #x26FF)))  ; Symbols
      (set-char-table-range char-width-table range 2))))
```

**What This Does**:
- GUI: Use Noto Color Emoji, Symbola for symbols
- Terminal: Force emoji width=2, use Noto Emoji (no color)
- Prevents single-width emoji from breaking alignment

### 3. Emoji Best Practices

**In Source Code**: Avoid emoji in comments (use ASCII alternatives)

**In Documentation**:
- ✅ Markdown files: Emoji OK (rendered outside terminal)
- ❌ Terminal display: Emoji can break layout
- 💡 Use sparingly in commit messages

**Current Config**:
```elisp
;; Emoji width ranges explicitly set to 2
(setq face-font-rescale-alist
      '(("Noto Color Emoji" . 0.9)
        ("Noto Emoji" . 0.9)))
```

## Testing Your Setup

### 1. Check Font Configuration

```elisp
M-x describe-fontset RET fontset-default RET
```

Look for:
- `hangul`: Should use your default monospace font
- `emoji`: Should use Noto Emoji (terminal) or Noto Color Emoji (GUI)

### 2. Test Character Widths

Create test buffer with:
```
ASCII:  AAAAAAAA (8 chars)
Hangul: 안녕하세 (4 chars = 8 columns)
Emoji:  😀😀😀😀 (4 chars = 8 columns)
Mixed:  A안B녕C (5 chars = 8 columns)
```

All lines should align vertically in terminal.

### 3. Test Window Splitting

```
C-x 2  ; Split horizontal
C-x 3  ; Split vertical
SPC q l ; Reload last session
```

Dashboard should display cleanly in narrow windows.

## Recommendations

### For Package Authors

1. **Avoid ASCII Art**: Use simple text or single-line banners
2. **Test in Terminal**: Always test with split windows
3. **Emoji Sparingly**: Assume width=2 in terminal
4. **Provide Toggles**: Let users disable fancy UI elements

### For Users

1. **Terminal Emulator**: Use one with good Unicode support (Alacritty, Kitty, WezTerm)
2. **Font Choice**: Use a font with CJK glyphs (Noto Sans Mono CJK, Sarasa Mono)
3. **Check Width**: `echo "😀" | wc -m` should return 2 in terminal

### Our Philosophy

> **Stability > Aesthetics** in terminal environments
>
> We prioritize reliable layout over visual effects, especially for CJK users who face daily width alignment issues.

## Technical Details

### Character Width Table

Emacs uses `char-width-table` to determine display width:
- Most ASCII: 1 column
- CJK characters: 2 columns
- Emoji: **varies by terminal** (we force 2)

Our config explicitly sets emoji ranges to width=2:

```elisp
(set-char-table-range char-width-table '(#x1F300 . #x1F6FF) 2)
```

### Why Not Use `unicode-width.el`?

Considered but rejected:
- ❌ Adds dependency
- ❌ Overkill for our needs
- ✅ Manual ranges are sufficient and explicit

### Fortune Integration

If you have `fortune` installed:

```bash
# Install fortune
sudo apt install fortune-mod fortunes

# Test
fortune
```

Our config uses 90% advice quotes, 10% random samples.

## Future Improvements

- [ ] Auto-detect terminal emoji support
- [ ] Per-terminal emulator font presets
- [ ] CJK-aware text wrapping functions
- [ ] Dashboard widget width detection

## References

- [Emacs Character Display](https://www.gnu.org/software/emacs/manual/html_node/elisp/Usual-Display.html)
- [Unicode East Asian Width](https://www.unicode.org/reports/tr11/)
- [Doom Emacs Dashboard](https://github.com/doomemacs/doomemacs/tree/master/modules/ui/doom-dashboard)

---

**Last Updated**: 2025-10-04
**Config Version**: dotdoom-starter v1.0
**Maintained by**: junghanacs
