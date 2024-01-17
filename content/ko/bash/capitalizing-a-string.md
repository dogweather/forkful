---
title:                "문자열 대문자로 변경하기"
html_title:           "Bash: 문자열 대문자로 변경하기"
simple_title:         "문자열 대문자로 변경하기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# 무엇 & 왜? 
문자열을 대문자로 변환하는 것을 캐피탈라이징이라고 합니다. 이는 보통 언어나 이름을 상관없이 모든 문자를 대문자로 바꾸는 것을 의미합니다. 프로그래머는 일반적으로 이를 사용하여 일관성 있게 텍스트를 표시하기 위해 또는 특정 검색 작업을 용이하게 하기 위해 사용합니다.

## 방법: 
아래 예시와 같이 Bash 코드 블록 안에 있습니다. 

```Bash
# Example 1: 변환할 문자열을 직접 지정
echo "hello, world" | tr '[:lower:]' '[:upper:]'

# 출력 결과:
HELLO, WORLD

# Example 2: 변수에 할당하여 사용하기
string="bash programming"
echo "$string" | tr '[:lower:]' '[:upper:]'

# 출력 결과:
BASH PROGRAMMING
```

## 깊게 들어가보기: 
이 기능의 역사적인 배경은 다양하지만, 보통 대문자로 표시하는게 더 눈에 띄고 가독성이 좋다는 이유로 인해 고안되었습니다. 또한 대소문자를 무시하고 검색을 수행하는 경우에도 유용합니다. 하지만 이 기능만으로는 모든 언어의 대문자로 변환하지 않으며, 일부 예외도 있을 수 있습니다.

## 참고 자료: 
더 많은 정보를 찾으시려면 아래의 링크를 참고하세요. 
- [Bash의 tr 명령어 설명서(Man Page)](https://linuxize.com/post/bash-compose-string)
- [GNU tr 유틸리티의 공식 문서](https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html)