---
date: 2024-01-26 04:11:35.020645-07:00
description: "REPL\uC740 Read-Eval-Print Loop\uC758 \uC57D\uC790\uB85C, \uB2E8\uC21C\
  \uD558\uBA74\uC11C\uB3C4 \uC0C1\uD638\uC791\uC6A9\uC801\uC778 \uCEF4\uD4E8\uD130\
  \ \uD504\uB85C\uADF8\uB798\uBC0D \uD658\uACBD\uC785\uB2C8\uB2E4. \uAC1C\uBC1C\uC790\
  \uB4E4\uC740 REPL\uC744 \uC0AC\uC6A9\uD558\uC5EC \uCF54\uB4DC\uB97C \uBE60\uB974\
  \uAC8C \uC791\uC131\uD558\uACE0 \uD14C\uC2A4\uD2B8\uD558\uBA70, \uBB38\uBC95\uC744\
  \ \uC2E4\uD5D8\uD558\uACE0, \uC804\uCCB4 \uC560\uD50C\uB9AC\uCF00\uC774\uC158\uC744\
  \ \uC0DD\uC131\uD558\uACE0 \uC2E4\uD589\uD558\uB294 \uBC88\uAC70\uB85C\uC6C0 \uC5C6\
  \uC774 \uD504\uB85C\uADF8\uB798\uBC0D \uAC1C\uB150\uC744 \uBC30\uC6B8 \uC218\u2026"
lastmod: '2024-02-25T18:49:52.476473-07:00'
model: gpt-4-0125-preview
summary: "REPL\uC740 Read-Eval-Print Loop\uC758 \uC57D\uC790\uB85C, \uB2E8\uC21C\uD558\
  \uBA74\uC11C\uB3C4 \uC0C1\uD638\uC791\uC6A9\uC801\uC778 \uCEF4\uD4E8\uD130 \uD504\
  \uB85C\uADF8\uB798\uBC0D \uD658\uACBD\uC785\uB2C8\uB2E4. \uAC1C\uBC1C\uC790\uB4E4\
  \uC740 REPL\uC744 \uC0AC\uC6A9\uD558\uC5EC \uCF54\uB4DC\uB97C \uBE60\uB974\uAC8C\
  \ \uC791\uC131\uD558\uACE0 \uD14C\uC2A4\uD2B8\uD558\uBA70, \uBB38\uBC95\uC744 \uC2E4\
  \uD5D8\uD558\uACE0, \uC804\uCCB4 \uC560\uD50C\uB9AC\uCF00\uC774\uC158\uC744 \uC0DD\
  \uC131\uD558\uACE0 \uC2E4\uD589\uD558\uB294 \uBC88\uAC70\uB85C\uC6C0 \uC5C6\uC774\
  \ \uD504\uB85C\uADF8\uB798\uBC0D \uAC1C\uB150\uC744 \uBC30\uC6B8 \uC218\u2026"
title: "\uC778\uD130\uB799\uD2F0\uBE0C \uC178 (REPL) \uC0AC\uC6A9\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇이며 왜인가?
REPL은 Read-Eval-Print Loop의 약자로, 단순하면서도 상호작용적인 컴퓨터 프로그래밍 환경입니다. 개발자들은 REPL을 사용하여 코드를 빠르게 작성하고 테스트하며, 문법을 실험하고, 전체 애플리케이션을 생성하고 실행하는 번거로움 없이 프로그래밍 개념을 배울 수 있습니다.

## 사용 방법:
Bash에서, 당신의 터미널은 본질적으로 REPL입니다. 명령을 입력하면, 이를 읽고, 평가하고, 결과를 출력한 다음, 다음 명령을 기다리며 반복합니다. 다음은 Bash를 REPL로 사용하는 예입니다:

```Bash
$ echo "Hello, World!"
Hello, World!
$ x=$((6 * 7))
$ echo $x
42
```

`$ ` 프롬프트 뒤에 입력하면, 출력은 다음 줄에 표시됩니다. 간단하죠?

## 심층 탐구
Bash는 Bourne Again SHell의 줄임말로, 많은 Unix 기반 시스템에서 기본 셸입니다. 이것은 1970년대 후반에 만들어진 원래 Bourne 셸의 업그레이드 버전입니다. Bash는 강력한 스크립팅 도구이지만, 그것의 대화형 모드는 당신이 명령어를 한 줄씩 실행할 수 있게 합니다.

대안을 고려할 때, 터미널에서 `python`을 입력함으로써 Python REPL, `node`와 함께하는 Node.js, 그리고 향상된 대화형 Python 셸인 IPython이 있습니다. 거의 모든 언어는 그들만의 REPL 구현을 가지고 있습니다.

밑바탕에서, REPL은 입력(명령어나 코드)을 파싱하고, 실행하며, stdout(당신의 화면)으로 결과를 반환하는 루프입니다. 주로 해당 언어의 인터프리터를 직접 사용합니다. 이러한 즉각적인 피드백은 배우기와 프로토타이핑에 아주 좋습니다.

## 또한 보기
- [공식 GNU Bash 문서](https://gnu.org/software/bash/manual/bash.html)
- [Learn Shell 대화형 튜토리얼](https://www.learnshell.org/)
- [IPython 공식 웹사이트](https://ipython.org/)
- [REPL.it](https://replit.com/): 다중 언어 온라인 REPL (단지 Bash만이 아닙니다!)
