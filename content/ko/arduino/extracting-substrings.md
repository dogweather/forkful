---
title:                "부분 문자열 추출"
html_title:           "Arduino: 부분 문자열 추출"
simple_title:         "부분 문자열 추출"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

문자열에서 서브스트링(Substring) 추출은 특정 부분 문자열을 얻기 위한 행위입니다. 프로그래머들이 이 작업을 수행하는 이유는 데이터 처리를 위해서나 더 의미있는 정보를 얻기 위함입니다.

## 어떻게 하는가:

```Arduino 
String str = "Arduino 프로그래밍!";
String result = str.substring(0, 7);

// 결과 출력
Serial.println(result); 
```
이 코드는 "Arduino 프로그래밍!" 문자열에서 "Arduino "를 추출합니다.

## 심층 탐구

(1) 잠깐 역사를 들여다보면, 서브스트링 추출은 오래 전부터 컴퓨터 프로그래밍의 핵심 요소 중 하나였습니다. 텍스트 처리, 파싱 등 많은 영역에서 사용되었습니다. 

(2) 이 외에도, 서브스트링의 추출을 위한 다른 방법들이 있습니다. 예를 들어, 정규 표현식을 이용할 수 있습니다. 

(3) 서브스트링을 추출하는 알고리즘은 매우 간단합니다. 주요 작업은 기본 문자열에서 원하는 위치의 문자를 복사하는 것입니다. 간단히 말해, 필요한 부분을 '자르는' 작업입니다.

## 참고 자료

다른 관련 정보를 찾아 보려면 아래의 링크를 참조하세요.

* [아두이노 공식 페이지](https://www.arduino.cc)
* [아두이노 공식 문서: substring()](https://www.arduino.cc/en/Tutorial/StringSubstring)
* [아두이노 서브스트링 관련 블로그 포스트](https://www.megunolink.com/articles/3-tutorials/70-arduino-string-manipulation-using-minimal-ram)