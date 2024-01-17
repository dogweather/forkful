---
title:                "문자열의 길이 찾기"
html_title:           "Bash: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 무엇과 왜?
이 글에서는 문자열의 길이를 찾는 것이 무엇인지 그리고 프로그래머들이 왜 이를 하는지에 대해 설명합니다.

## 방법:
다음은 Bash에서 문자열의 길이를 찾는 방법에 대한 코딩 예제와 출력 결과입니다. 

```Bash
# 변수에 문자열 할당하기
str="Hello World"

# 문자열의 길이 찾기
echo "문자열의 길이: ${#str}"

# 출력 결과:
# 문자열의 길이: 11
```

## 깊이 파고들기:
- 역사적 배경: 문자열의 길이를 찾는 기능은 오래전부터 프로그래밍 언어의 기본 기능으로 존재해왔습니다.
- 대안: Bash에서는 다른 방법으로도 문자열의 길이를 찾을 수 있지만, 위의 예제처럼 변수와 문자열의 길이를 함께 사용하는 것이 가장 간단한 방법입니다.
- 구현 세부사항: Bash에서 문자열의 길이는 ```${#str}``` 형식을 사용하여 찾을 수 있습니다. 이때 변수의 앞에 "$"를 붙이고 중괄호 {} 안에 #을 포함시켜주면 됩니다.

## 관련 정보:
- [Bash 공식 문서](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html) - 변수와 문자열의 길이 관련 공식 설명서입니다.
- [Bash 쉘 스크립트 튜토리얼](https://www.shellscript.sh/index.html) - 문자열의 길이를 찾는 것뿐만 아니라 Bash에서의 기초적인 쉘 스크립트에 대해 배울 수 있는 사이트입니다.