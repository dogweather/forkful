---
title:    "Arduino: 문자열 대문자 변환하기"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

# 왜

문자열을 대문자로 바꾸는 것이 왜 중요한지 궁금하신가요? 이 글을 통해 올바른 프로그래밍 방법을 배우고 여러분의 아두이노 프로젝트에 적용해보세요.

## 이렇게 하세요

문자열을 대문자로 바꾸는 것은 아두이노 프로그래밍에서 자주 사용되는 작업 중 하나입니다. 이 작업을 수행하는 가장 간단한 방법은 "toUpperCase()" 함수를 사용하는 것입니다. 아래 코드를 참고해보세요.

```Arduino
String str = "hello world";
str.toUpperCase();
Serial.println(str); // 출력 결과: HELLO WORLD
```

위 코드에서 "toUpperCase()" 함수는 문자열 "hello world"를 대문자로 변환하여 "HELLO WORLD"로 저장합니다. 그리고 "Serial.println()" 함수를 사용하여 변환된 문자열을 시리얼 모니터에 출력합니다.

## 심층 탐구

문자열을 대문자로 바꾸는 것이 어떻게 가능한지 자세하게 알아보겠습니다. 아두이노는 "ASCII" 문자 코드를 사용하므로 이에 대해 간단히 설명하겠습니다. "A"는 65의 ASCII 코드이고 "a"는 97의 ASCII 코드입니다. 이를 이용하여 간단한 알고리즘을 만들어 대문자로 변환할 수 있습니다.

아래의 코드는 "ASCII" 코드를 이용해 바꾼 대문자를 문자열로 반환하는 함수 예시입니다.

```Arduino
String toUpperCase(String str){
  String result = ""; // 변환된 결과를 저장할 변수
  for (int i = 0; i < str.length(); i++){
    // 문자열 내 각 문자의 ASCII 코드값 가져오기
    int code = int(str.charAt(i));
    // 소문자일 경우에만 변환하기
    if (code >= 97 && code <= 122){
      result += char(code - 32); // 대문자의 ASCII 코드는 소문자의 ASCII코드에서 32를 뺀 값
    }
    else{
      result += str.charAt(i); // 대문자가 아닐 경우 그대로 저장
    }
  }
  return result; // 변환된 대문자 문자열 반환
}
```

이렇게 자세하게 알아보면 문자열을 대문자로 바꾸는 것이 얼마나 간단한 작업인지 알 수 있습니다.

## 관련 링크

- [Arduino 공식 홈페이지](https://www.arduino.cc/)
- [ASCII 코드 표](http://www.asciitable.com/)
- [마크다운(markdown) 사용법](https://guides.github.com/features/mastering-markdown/)