---
aliases:
- /ko/bash/checking-if-a-directory-exists/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:55.629968-07:00
description: "Bash \uD504\uB85C\uADF8\uB798\uBC0D\uC5D0\uC11C \uB514\uB809\uD1A0\uB9AC\
  \uAC00 \uC874\uC7AC\uD558\uB294\uC9C0 \uD655\uC778\uD558\uB294 \uAC83\uC740 \uD30C\
  \uC77C \uC791\uC5C5\uC744 \uC218\uD589\uD558\uAE30 \uC804\uC5D0 \uB514\uB809\uD1A0\
  \uB9AC\uC758 \uC874\uC7AC\uB97C \uAC80\uC99D\uD558\uB294 \uD544\uC218\uC801\uC778\
  \ \uC81C\uC5B4 \uBA54\uCEE4\uB2C8\uC998\uC785\uB2C8\uB2E4. \uC774\uB7EC\uD55C \uAC80\
  \uC0AC\uB294 \uC874\uC7AC\uD558\uC9C0 \uC54A\uB294 \uB514\uB809\uD1A0\uB9AC\uC5D0\
  \ \uC561\uC138\uC2A4\uD558\uAC70\uB098 \uC218\uC815\uD558\uB824\uACE0 \uC2DC\uB3C4\
  \uD558\uB294 \uC624\uB958\uB97C \uC608\uBC29\uD558\uC5EC \uC2A4\uD06C\uB9BD\uD2B8\
  \ \uC2E4\uD589\uC744 \uB354 \uC6D0\uD65C\uD558\uACE0 \uC608\uCE21 \uAC00\uB2A5\uD558\
  \uAC8C\u2026"
lastmod: 2024-02-18 23:09:06.508158
model: gpt-4-0125-preview
summary: "Bash \uD504\uB85C\uADF8\uB798\uBC0D\uC5D0\uC11C \uB514\uB809\uD1A0\uB9AC\
  \uAC00 \uC874\uC7AC\uD558\uB294\uC9C0 \uD655\uC778\uD558\uB294 \uAC83\uC740 \uD30C\
  \uC77C \uC791\uC5C5\uC744 \uC218\uD589\uD558\uAE30 \uC804\uC5D0 \uB514\uB809\uD1A0\
  \uB9AC\uC758 \uC874\uC7AC\uB97C \uAC80\uC99D\uD558\uB294 \uD544\uC218\uC801\uC778\
  \ \uC81C\uC5B4 \uBA54\uCEE4\uB2C8\uC998\uC785\uB2C8\uB2E4. \uC774\uB7EC\uD55C \uAC80\
  \uC0AC\uB294 \uC874\uC7AC\uD558\uC9C0 \uC54A\uB294 \uB514\uB809\uD1A0\uB9AC\uC5D0\
  \ \uC561\uC138\uC2A4\uD558\uAC70\uB098 \uC218\uC815\uD558\uB824\uACE0 \uC2DC\uB3C4\
  \uD558\uB294 \uC624\uB958\uB97C \uC608\uBC29\uD558\uC5EC \uC2A4\uD06C\uB9BD\uD2B8\
  \ \uC2E4\uD589\uC744 \uB354 \uC6D0\uD65C\uD558\uACE0 \uC608\uCE21 \uAC00\uB2A5\uD558\
  \uAC8C\u2026"
title: "\uB514\uB809\uD1A0\uB9AC\uAC00 \uC874\uC7AC\uD558\uB294\uC9C0 \uD655\uC778\
  \uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇을, 왜?

Bash 프로그래밍에서 디렉토리가 존재하는지 확인하는 것은 파일 작업을 수행하기 전에 디렉토리의 존재를 검증하는 필수적인 제어 메커니즘입니다. 이러한 검사는 존재하지 않는 디렉토리에 액세스하거나 수정하려고 시도하는 오류를 예방하여 스크립트 실행을 더 원활하고 예측 가능하게 만드는 데 중요합니다.

## 방법:

기본적으로, Bash는 조건문과 `-d` 연산자를 사용하여 디렉토리의 존재 여부를 확인할 수 있습니다. 아래는 이러한 확인을 수행하는 방법을 보여주는 간단한 예시입니다.

```bash
if [ -d "/path/to/directory" ]; then
    echo "디렉토리가 존재합니다."
else
    echo "디렉토리가 존재하지 않습니다."
fi
```

샘플 출력 (디렉토리가 존재하는 경우):
```
디렉토리가 존재합니다.
```

샘플 출력 (디렉토리가 존재하지 않는 경우):
```
디렉토리가 존재하지 않습니다.
```

보다 복잡한 스크립트의 경우, 디렉토리가 존재하지 않으면 생성하는 작업과 같은 다른 연산과 함께 확인을 결합하는 것이 일반적입니다:

```bash
DIR="/path/to/directory"
if [ -d "$DIR" ]; then
    echo "$DIR 이(가) 존재합니다."
else
    echo "$DIR 이(가) 존재하지 않습니다. 지금 생성합니다..."
    mkdir -p "$DIR"
    echo "$DIR 생성됨."
fi
```

샘플 출력 (디렉토리가 존재하지 않는 경우 생성된 후):
```
/path/to/directory 이(가) 존재하지 않습니다. 지금 생성합니다...
/path/to/directory 생성됨.
```

Bash 스스로 이러한 검사를 위해 충분히 강력하고 효율적인 도구를 제공함에도 불구하고, 이 작업에 특화된 인기 있는 서드파티 라이브러리는 없습니다.
