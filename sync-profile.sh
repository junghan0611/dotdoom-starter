#!/usr/bin/env bash
# Doom Emacs Starter Sync 스크립트 (분리된 설치 버전)

echo "=== Doom Emacs Starter Sync ==="

# 환경 변수 설정
export DOOMDIR="$HOME/repos/gh/dotdoom-starter"
DOOM_BIN="$HOME/doomemacs-starter/bin/doom"

echo "DOOMDIR: $DOOMDIR"
echo "Doom Bin: $DOOM_BIN"

# 패키지 동기화 (대화형 프롬프트 없이)
echo ""
echo "Syncing starter packages..."
echo $DOOM_BIN
$DOOM_BIN sync

echo ""
echo "=== Sync Complete ==="
echo ""
echo "사용법:"
echo "  1. 직접 실행: ./start-emacs.sh"
echo "  2. 환경변수 설정: export DOOMPROFILE=starter && emacs"
echo "  3. 명령줄 옵션: emacs --profile starter"

