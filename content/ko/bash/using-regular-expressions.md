---
title:                "정규식을 사용하는 방법"
html_title:           "Bash: 정규식을 사용하는 방법"
simple_title:         "정규식을 사용하는 방법"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

regular expressions를 사용하는 것은 무엇인지 설명해보자면, 정규 표현식은 문자열에서 특정한 패턴을 찾는 데 사용되는 표현 방식입니다. 프로그래머들은 이것을 사용하는 이유는, 예를 들어 문자열 검색, 문서 분석 또는 데이터 추출 같은 작업을 더 쉽게 할 수 있기 때문입니다.

## 방법:

`Bash` 블록 안에 코드 예제와 결과물을 포함하여 코딩 예제를 설명하겠습니다. 

```Bash
# "Hello, World!" 문자열에서 'o'를 찾는 예제
$ echo "Hello, World!" | grep 'o'
o
```

```Bash
# 임의의 텍스트 파일에서 이메일 주소를 추출하는 예제
$ cat input.txt
Hi, my email is example@gmail.com and I love coding!
$ grep -o '[a-zA-Z0-9_.-]*@[a-zA-Z]*.[a-z]{2,3}' input.txt
example@gmail.com
```

## 깊게 파고들기:

정규 표현식은 1950년대에 톰슨 외에도 여러 고급 프로그래머들이 개발을 한 이래로, 프로그래밍에 매우 유용한 기능으로 자리 잡고 있습니다. 대안으로는 Shell 스크립트에서 문자열 관련 작업에 자주 사용되는 `awk`, `sed` 등의 명령어가 있으며, 이러한 명령어들도 정규 표현식을 기반으로 작동합니다. 구현 상세 정보로는 `Regex` 엔진이 작동하는 방식, 파서와 어떻게 동작하는지 등이 포함될 수 있습니다.

## 관련 자료:

- [Bash 정규 표현식 소개](https://www.gnu.org/software/bash/manual/html_node/Pattern-Matching.html)
- [Regex 테스트와 배움터](https://regex101.com/)
- [정규 표현식을 대체할 수 있는 명령어들](https://www.lifewire.com/search-commands-by-specific-criteria-2201117)