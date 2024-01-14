---
title:                "Fish Shell: 문자열의 길이 찾기"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# 왜
문자열의 길이를 찾는 것에 대해 관심을 가지는 이유는, 우리가 다루는 데이터의 크기를 파악하고 다른 작업에 적절하게 활용할 수 있기 때문입니다.

## 어떻게
```Fish Shell```에서 문자열의 길이를 찾는 방법은 매우 간단합니다. 명령줄에 다음과 같이 입력하여 문자열을 생성합니다.
```
set hello "안녕하세요"
```
이제 길이를 찾고 싶은 문자열 변수를 ```string length``` 명령어에 전달합니다.
```
string length $hello
```
위의 예제에서는 6이라는 숫자가 출력될 것입니다. 즉, "안녕하세요"라는 문자열의 길이를 나타냅니다.

## 깊이 파고들기
문자열의 길이를 찾는 것이 어떤 원리로 이루어지는지 궁금할 수 있습니다. 내부적으로, ```string length``` 명령어는 문자열을 배열로 변환하고 배열의 요소 개수를 세어서 결과를 반환합니다. 또한 이 명령어는 UTF-8 인코딩을 지원하므로 다국어 문자열에 대해서도 정확한 길이를 반환할 수 있습니다.

# See Also
- [Fish Shell 공식 문서](https://fishshell.com/docs/current/commands.html#string-length)
- [KoreanFish 가이드](https://github.com/samebl0ck/koreanfish)
- [Fish Shell Stack Overflow 페이지](https://stackoverflow.com/tags/fish)