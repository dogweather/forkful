---
title:                "문자열의 길이 찾기"
html_title:           "Lua: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# 문자열 길이 찾기: Bash 프로그래밍 꿀팁

## 무엇이고 왜 필요한가?
문자열의 길이를 찾는 것은 주어진 문자열에 포함된 문자의 수를 구하는 작업입니다. 프로그래머들이 이 작업을 수행하는 이유는 문자열 조작, 인코딩 검사, 버퍼 크기 계산 등 다양한 자료 처리와 효율적인 메모리 관리를 위해 필요하기 때문입니다.

## 어떻게 하나요?
Bash에서는 `{$#변수명}` 형태로 문자열의 길이를 쉽게 찾을 수 있습니다. 이런 간단한 예시를 봅시다.

```Bash
my_string="안녕하세요, 베시!"
echo ${#my_string}
```
이 코드를 실행하면 출력 결과로 `15`가 나옵니다.

## 딥 다이브
Bash에서 문자열 길이를 찾는 방법은 간단히는 ${#변수명} 구문을 사용하는 것이지만, 이는 POSIX 쉘 프로그래밍 표준에는 포함되어 있지 않습니다. 이 방법은 Bash나 Korn 쉘에서 지원하는 확장 기능입니다.

대안으로 `wc -m` 명령을 사용하여 문자열 길이를 구할 수 있지만, 이 방법은 개행 문자도 개수에 포함합니다. 따라서 실제 문자열 길이와 1 차이가 날 수 있습니다.

원하는 문자열의 길이를 찾는 데 있어서는 명확한 정답이 없습니다. 문맥과 응용 프로그램의 요구에 따라 적합한 방법을 선택하는 것이 중요합니다.

## 참조 자료
1. [GNU Bash Manual : Shell Parameter Expansion](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)
2. [How to find length of string in bash](https://www.cyberciti.biz/faq/unix-linux-bash-find-out-length-of-string/)
3. [Using wc command in Linux](https://www.geeksforgeeks.org/wc-command-linux-examples/)