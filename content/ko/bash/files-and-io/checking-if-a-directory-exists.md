---
title:                "디렉토리가 존재하는지 확인하기"
aliases:
- /ko/bash/checking-if-a-directory-exists/
date:                  2024-02-03T19:06:55.629968-07:00
model:                 gpt-4-0125-preview
simple_title:         "디렉토리가 존재하는지 확인하기"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
