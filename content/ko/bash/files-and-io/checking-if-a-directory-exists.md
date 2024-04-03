---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:55.629968-07:00
description: "\uBC29\uBC95: \uAE30\uBCF8\uC801\uC73C\uB85C, Bash\uB294 \uC870\uAC74\
  \uBB38\uACFC `-d` \uC5F0\uC0B0\uC790\uB97C \uC0AC\uC6A9\uD558\uC5EC \uB514\uB809\
  \uD1A0\uB9AC\uC758 \uC874\uC7AC \uC5EC\uBD80\uB97C \uD655\uC778\uD560 \uC218 \uC788\
  \uC2B5\uB2C8\uB2E4. \uC544\uB798\uB294 \uC774\uB7EC\uD55C \uD655\uC778\uC744 \uC218\
  \uD589\uD558\uB294 \uBC29\uBC95\uC744 \uBCF4\uC5EC\uC8FC\uB294 \uAC04\uB2E8\uD55C\
  \ \uC608\uC2DC\uC785\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.503003-06:00'
model: gpt-4-0125-preview
summary: "\uAE30\uBCF8\uC801\uC73C\uB85C, Bash\uB294 \uC870\uAC74\uBB38\uACFC `-d`\
  \ \uC5F0\uC0B0\uC790\uB97C \uC0AC\uC6A9\uD558\uC5EC \uB514\uB809\uD1A0\uB9AC\uC758\
  \ \uC874\uC7AC \uC5EC\uBD80\uB97C \uD655\uC778\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4\
  ."
title: "\uB514\uB809\uD1A0\uB9AC\uAC00 \uC874\uC7AC\uD558\uB294\uC9C0 \uD655\uC778\
  \uD558\uAE30"
weight: 20
---

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
