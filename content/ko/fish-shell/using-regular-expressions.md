---
title:                "정규식 사용하기"
html_title:           "Fish Shell: 정규식 사용하기"
simple_title:         "정규식 사용하기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

<div dir="rtl">
## 왜?

정규 표현식은 우리가 원하는 결과를 얻기 위해 문자열을 검색하고 분석하는 강력한 도구입니다. 이를 사용함으로써 우리는 텍스트 처리 작업을 더욱 효율적이고 정확하게 할 수 있습니다.

## 사용 방법

```Fish Shell```에서 정규 표현식을 사용하는 방법은 간단합니다. 우선, ```=``` 연산자를 사용하여 문자열을 변수에 할당합니다. 그리고 변수 앞에 ```_```를 붙여 정규 표현식을 작성합니다.

```
set str "Hello, World!"
set regex _Hello

echo $str =~ $regex
```

이렇게 하면 해당 문자열이 정규 표현식과 일치하는지를 확인할 수 있습니다. 그리고 동적으로 정규 표현식을 사용하기 위해서는 ```eval``` 함수를 사용할 수 있습니다.

```
eval echo (echo $str =~ $regex)
```

정규 표현식의 결과를 변수에 할당하여 다른 작업에 활용할 수도 있습니다.

```
set result (echo $str =~ $regex)

if test $result = 1
    echo "일치합니다!"
else
    echo "일치하지 않습니다."
end
```

## 더 깊게 알아보기

정규 표현식은 다양한 메타 문자와 패턴을 사용하여 문자열을 검색하고 처리하는 강력한 도구입니다. 수 많은 패턴과 메타 문자를 모두 소개하기에는 너무 많으므로, 자주 사용되는 몇 가지를 예시를 통해 살펴보겠습니다.

### 메타 문자

| 메타 문자 | 설명 |
| ---------| -----|
| ```^``` | 문자열의 시작 부분과 일치 |
| ```$``` | 문자열의 끝 부분과 일치 |
| ```.``` | 개행 문자를 제외한 모든 문자와 일치 |
| ```*``` | 앞 문자가 0번 이상 반복되는 패턴과 일치 |
| ```+``` | 앞 문자가 1번 이상 반복되는 패턴과 일치 |
| ```?``` | 앞 문자가 0번 또는 1번 나오는 패턴과 일치 |

### 패턴

| 패턴 | 설명 |
| ----| -----|
| ```[0-9]``` | 0부터 9까지의 숫자와 일치 |
| ```[a-z]``` | 소문자 알파벳과 일치 |
| ```[A-Z]``` | 대문자 알파벳과 일치 |
| ```[a-zA-Z0-9]``` | 소문자, 대문자, 숫자와 일치 |
| ```[aeiou]``` | 소문자 모음과 일치 |
| ```[AEIOU]``` | 대문자 모음과 일치 |

더 많은 메타 문자와 패턴은 인터넷에서 찾아볼 수 있습니다. 또한, 한글을 포함한 다국어 문자열도 정규 표현식으로 처리할 수 있으니 참고해주세요.

## 관련 자료

[Fish Shell Regular Expression 홈페이지](https://fishshell.com/docs/current/cmds/set.html)

[정규 표현식 테스트 사이트](https://regexr.com/) 

[정규 표현식 튜토리얼 영상](https://www.youtube.com/playlist?list=PLVsCAX67pch-tP47IZNDHSiVTRx6D6hJZ)