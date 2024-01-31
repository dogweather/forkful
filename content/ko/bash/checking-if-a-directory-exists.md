---
title:                "디렉토리의 존재 여부 확인하기"
date:                  2024-01-19
simple_title:         "디렉토리의 존재 여부 확인하기"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
디렉터리 존재 여부를 확인하는 것은 파일 시스템에서 특정 폴더가 있는지를 확인하는 과정입니다. 프로그래머들은 불필요한 오류 방지, 데이터 작성 전 사전 조건 설정, 그리고 로직 흐름 제어를 위해 이를 수행합니다.

## How to: (방법)
```Bash
# 디렉터리 존재 여부 확인 방법
if [ -d "/path/to/dir" ]; then
  echo "Directory exists."
else
  echo "Directory does not exist."
fi

# 샘플 출력
Directory exists.
```

```Bash
# 디렉터리가 없으면 만드는 방법
mkdir -p "/path/to/dir"
```

## Deep Dive (심층 분석)
Bash에서 디렉터리가 존재하는지 확인하는 것은 오래된 관습입니다. `-d` 플래그는 1980년대 초부터 존재하며, 여전히 많은 스크립트에서 사용됩니다. 대안으로 `[[ ]]` 구문이 있으며, 더 현대적이고 풍부한 기능을 제공합니다. 예를 들어, 다음과 같습니다:

```Bash
# [[ ]]를 이용한 더 현대적인 접근 방식
if [[ -d "/path/to/dir" ]]; then
  echo "Directory exists."
else
  echo "Directory does not exist."
fi
```

이 방식은 `[ ]`보다 몇 가지 이점을 가지고 있습니다: 단어 분할(word splitting)과 경로 확장(pathname expansion)이 발생하지 않으며, 보다 복잡한 조건부 표현이 가능합니다.

디렉터리 생성 시, `mkdir -p` 명령어는 중첩된 디렉터리를 생성하면서, 이미 존재하는 경우에는 오류를 뱉지 않습니다. 이는 효율적인 파일 구조 관리를 위해 중요합니다.

## See Also (참고 자료)
- Bash Manual: https://www.gnu.org/software/bash/manual/
- Advanced Bash-Scripting Guide: https://www.tldp.org/LDP/abs/html/
- StackOverflow: How to check if a directory exists in a Bash shell script: https://stackoverflow.com/questions/59838/check-if-a-directory-exists-in-a-shell-script
