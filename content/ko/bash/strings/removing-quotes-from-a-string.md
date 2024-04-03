---
date: 2024-01-26 03:38:00.042168-07:00
description: "\uBC29\uBC95: Bash\uC5D0\uC11C\uB294 \uC5EC\uB7EC \uBC29\uBC95\uC73C\
  \uB85C \uBB38\uC790\uC5F4\uC5D0\uC11C \uB530\uC634\uD45C\uB97C \uC81C\uAC70\uD560\
  \ \uC218 \uC788\uB2E4. \uC5EC\uAE30 \uBA87 \uAC00\uC9C0 \uBE60\uB978 \uC608\uC2DC\
  \uB97C \uC18C\uAC1C\uD55C\uB2E4."
lastmod: '2024-03-13T22:44:55.461802-06:00'
model: gpt-4-0125-preview
summary: "Bash\uC5D0\uC11C\uB294 \uC5EC\uB7EC \uBC29\uBC95\uC73C\uB85C \uBB38\uC790\
  \uC5F4\uC5D0\uC11C \uB530\uC634\uD45C\uB97C \uC81C\uAC70\uD560 \uC218 \uC788\uB2E4\
  ."
title: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB530\uC634\uD45C \uC81C\uAC70\uD558\uAE30"
weight: 9
---

## 방법:
Bash에서는 여러 방법으로 문자열에서 따옴표를 제거할 수 있다. 여기 몇 가지 빠른 예시를 소개한다:

```Bash
#!/bin/bash

# 변수 치환을 사용해 단일 및 이중 따옴표 모두 제거
STRING="\"Hello, World!\""
echo ${STRING//\"}

# `tr`을 사용하여 따옴표 삭제
STRING="'Hello, World!'"
echo $STRING | tr -d "\'"

# `sed`를 사용하여 따옴표 삭제
STRING="\"Hello, World!\""
echo $STRING | sed 's/"//g'
```

샘플 출력:

```
Hello, World!
Hello, World!
Hello, World!
```

## 심층 탐색
옛날에는 `tr`과 `sed` 같은 Unix 명령어들이 텍스트 처리를 위한 주요 도구였다. 따옴표 제거와 같은 텍스트 변환 작업을 처리하는 데 있어 그들의 유연성과 강력함 때문에 오늘날에도 여전히 사용되고 있다. 이들은 어떤 쉘 스크립터의 도구 상자에도 필수적이다.

Bash 자체도 발전하여 변수 치환은 소규모 문자열 조작을 위한 또 하나의 단순함을 추가했다. 이는 외부 바이너리로 파이프를 하지 않아도 되게 하여 스크립트를 조금 더 효율적으로 만든다.

`tr`은 문자를 삭제하는 데 좋지만 더 복잡한 패턴을 처리하지는 못한다. 반면에 `sed`는 정규 표현식을 사용하므로 때로는 과잉일 수 있고 단순한 작업에 대해 더 느릴 수 있다.

이러한 방법들 중에서 선택하는 것은 특정 케이스에 따라 다르다. 여러 종류의 따옴표를 제거해야 하며 이미 Bash 스크립트의 맥락에 있을 경우, 그 단순함 때문에 변수 치환을 사용하는 것이 명백한 선택이다. 하지만 텍스트 스트림이나 다중 행 데이터를 변환해야 한다면, `tr`과 `sed`이 당신의 친구들이다.

## 참고 자료:
- GNU Bash 매뉴얼, 특히 매개변수 확장 및 쉘 매개변수 확장에 대한 섹션: https://www.gnu.org/software/bash/manual/
- `tr` 명령어 매뉴얼: https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html
- `sed` 스트림 편집기 개요: https://www.gnu.org/software/sed/manual/sed.html
