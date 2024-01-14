---
title:                "Bash: 부분 문자열 추출하기"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜

서브스트링을 추출하는 이유는 무엇일까요? 이 작업은 문자열에서 특정 문자 또는 단어를 찾기 위해 자주 사용되며 유용한 정보를 추출하는 데 도움이 됩니다.

## 하우투

서브스트링을 추출하는 방법은 간단합니다. 다음의 Bash 코드를 사용하면 됩니다.

```Bash
# 문자열 변수 정의
str="이것은 문자열 예제입니다"

# 서브스트링 추출
echo ${str:0:6} # 출력 : 이것은
echo ${str:6} # 출력 : 문자열 예제입니다
```

위의 코드에서 `${str:start:length}` 형식으로 사용하며, `start` 위치부터 `length` 길이만큼의 서브스트링을 추출합니다. `start` 위치는 0부터 시작하며, `length`를 생략하면 `start` 위치부터 문자열 끝까지 추출합니다.

## 딥 다이브

서브스트링 추출에 대해 더 깊이 알아보겠습니다. Bash는 다음과 같은 방법으로 서브스트링을 추출할 수 있습니다.

- `${str:start:length}` : 앞서 언급한 방법으로 추출
- `${str: -start:length}` : 문자열의 뒤에서부터 `start` 위치부터 `length` 길이만큼 추출 (공백도 포함)
- `${str: -start:-end}` : 뒤에서부터 `start` 위치부터 `end` 위치까지 추출
- `${str#substring}` : `substring`과 일치하는 처음의 부분 문자열을 제거하고 나머지 문자열을 반환
- `${str%substring}` : `substring`과 일치하는 마지막의 부분 문자열을 제거하고 나머지 문자열을 반환

더 자세한 내용은 [Bash 서브스트링 추출 방법](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)을 참고하세요.

## 봐도 좋아요

- [Bash String Manipulation](https://www.linuxjournal.com/content/bash-string-manipulation): Bash에서 문자열을 조작하는 방법에 대해 자세하게 설명한 글입니다.
- [Bash String Operations](https://www.gnu.org/software/bash/manual/html_node/String-Operations.html): Bash에서 제공하는 다양한 문자열 처리 방법을 설명한 공식 문서입니다.
- [Bash Substring Explained with Examples](https://www.shell-tips.com/bash/substring-extraction/): Bash에서 서브스트링을 추출하는 다양한 방법과 예제를 제공하는 글입니다.