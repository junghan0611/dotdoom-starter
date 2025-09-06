#!/bin/bash
# Doom Emacs Starter Profile 실행 스크립트
# 터미널 모드 중심 - 노트북과 Termux 환경 모두 지원
#
# 사용법:
#   ./start-emacs.sh          # 터미널 모드 (기본)
#   ./start-emacs.sh --gui    # GUI 모드로 실행
#   ./start-emacs.sh file.txt # 파일 열기 (터미널 모드)

# emacs-stable 우선 사용
if command -v emacs-stable &> /dev/null; then
    EMACS_BIN="emacs-stable"
else
    EMACS_BIN="emacs"
fi

# Doom profile 설정
export DOOMPROFILE=starter
export DOOMDIR="$HOME/repos/gh/dotdoom-starter"

# Termux 환경 감지
if [ -n "$TERMUX_VERSION" ]; then
    echo "Termux 환경 감지됨"
    # Termux 특수 경로 설정 (필요시)
    export TERMUX_ENV=true
fi

# GUI 모드 체크 (--gui 옵션이 있으면 GUI로 실행)
if [[ " $@ " =~ " --gui " ]]; then
    echo "Starting Doom Emacs (GUI mode) with starter profile..."
    # --gui 옵션 제거하고 실행
    ARGS="${@//--gui/}"
    MODE=""
else
    echo "Starting Doom Emacs (Terminal mode) with starter profile..."
    # 기본은 터미널 모드 (-nw)
    MODE="-nw"
    ARGS="$@"
fi

echo "EMACS: $EMACS_BIN"
echo "DOOMDIR: $DOOMDIR"
echo "DOOMPROFILE: $DOOMPROFILE"

# Emacs 실행
exec $EMACS_BIN $MODE $ARGS