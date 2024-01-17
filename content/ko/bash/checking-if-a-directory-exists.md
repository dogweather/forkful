---
title:                "디렉토리가 존재하는지 확인하기"
html_title:           "Bash: 디렉토리가 존재하는지 확인하기"
simple_title:         "디렉토리가 존재하는지 확인하기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 무엇인가요? 
디렉토리가 존재하는지 확인하는 것은 프로그래머가 자주 하는 작업 중 하나입니다. 디렉토리가 존재하는지 확인하는 이유는 현재 작업 중인 파일이나 폴더가 제대로 위치해 있는지를 확인하기 위해서입니다.

## 하는 방법:
디렉토리가 존재하는지를 확인하는 방법은 다양하지만, 가장 간단한 방법은 `ls` 명령어를 사용하는 것입니다. 만약 `ls` 명령어의 결과에 해당 디렉토리가 나타나지 않는다면, 해당 디렉토리는 존재하지 않는 것으로 간주할 수 있습니다. 그리고 이를 Bash 스크립트로 작성하면 아래와 같이 됩니다.

```bash
if ls <디렉토리명> >/dev/null 2>&1; then
  echo "디렉토리가 존재합니다."
else
  echo "해당 디렉토리는 존재하지 않습니다."
fi
```

`>/dev/null 2>&1`는 `ls`의 결과를 출력하지 않기 위해 사용되는 명령어입니다. 만약 이 부분이 없다면 `ls`의 결과가 터미널에 출력되게 됩니다.

위 예시에서 `if`문에서 사용한 `ls` 명령어의 출력 결과는 `0`이 될 것입니다. `0`은 성공적으로 실행했다는 것을 의미하며, 이를 통해 해당 디렉토리가 존재하는지를 확인할 수 있습니다.

## 깊게 들어가보면:
### 역사적 배경:
디렉토리가 존재하는지 확인하는 기능은 Unix 시스템에서 많이 사용되었던 기능 중 하나입니다. Unix 시스템에서는 디렉토리가 존재하는지 확인할 때 `test` 명령어를 사용하였으며, 현재의 Bash에서는 `test` 명령어 대신 `ls` 명령어를 사용합니다.

### 다른 대안:
Bash에서 디렉토리가 존재하는지 확인하는 다른 방법으로는 `[[ -d <디렉토리명> ]]`를 사용하는 방법이 있습니다. 이 방법은 `-d` 옵션을 통해 디렉토리가 존재하는지를 확인하는 것이기 때문에 더 직관적입니다.

## 관련 정보:
- [Bash 스크립트로 디렉토리가 존재하는지 확인하는 방법 (번역)](https://www.webservertalk.com/how-to-check-if-directory-exists-in-a-bash-script-korean/)
- [Bash Reference Manual - Bash Conditional Expressions](https://www.gnu.org/software/bash/manual/html_node/Bash-Conditional-Expressions.html)