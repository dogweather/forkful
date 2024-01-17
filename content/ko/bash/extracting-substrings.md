---
title:                "부분 문자열 추출하기"
html_title:           "Bash: 부분 문자열 추출하기"
simple_title:         "부분 문자열 추출하기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

# ## 무엇 & 왜?
Substrings를 추출하는 것은 문자열에서 부분 문자열을 원하는 비공백 문자로 분리하는 것을 말합니다. 프로그래머는 이를 통해 원하는 데이터에 쉽게 접근하고 조작할 수 있습니다.

# ## 방법:
Bash 코드 블록 내에 코딩 예제와 출력 결과를 포함한 두 가지 방법으로 Substrings를 추출하는 방법을 설명합니다.

### Case 1:
```Bash
text="Hello World!"
echo ${text:0:5}
```
출력 결과: Hello

### Case 2:
```Bash
filename="document.txt"
echo ${filename%.*}
```
출력 결과: document

# ## 깊게 파보기:
Substrings를 추출하는 기능은 처음에는 쉽게 다가올 수 있지만, 실제로는 프로그래밍 언어마다 다르고 구현 방법도 상이합니다. 예를 들어, Bash에서는 Parameter Expansion이라는 기능을 사용하여 Substrings를 추출할 수 있지만, Python에서는 문자열 슬라이싱을 통해 비슷한 결과를 얻을 수 있습니다. 이러한 차이점은 언어마다 제공하는 기능과 다양성을 보여줍니다.

Substrings 추출은 주로 다양한 데이터 처리 작업에서 사용됩니다. 예를 들어, 로그 파일에서 특정 단어가 포함된 행을 추출하거나, 특정 파일의 확장자를 제외한 파일 이름만 추출하는 등 다양한 용도로 활용될 수 있습니다.

# ## 관련 자료:
- [Bash 공식 문서](https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameter-Expansion): Bash에서 제공하는 Parameter Expansion에 대한 공식 문서입니다.
- [Python 문자열 슬라이싱](https://docs.python.org/3/library/stdtypes.html#text-sequence-type-str): Python에서 문자열 슬라이싱을 사용하여 Substrings를 추출하는 방법에 대한 공식 문서입니다.