---
date: 2024-01-20 17:34:15.810610-07:00
description: "How to: (\uBC29\uBC95) \uBB38\uC790\uC5F4 \uC5F0\uACB0\uC740 \uC544\uB450\
  \uC774\uB178\uC758 \uCD08\uAE30 \uBC84\uC804\uBD80\uD130 \uC0AC\uC6A9\uB418\uC5B4\
  \ \uC654\uC9C0\uB9CC, \uB3D9\uC801 \uBA54\uBAA8\uB9AC \uAD00\uB9AC\uAC00 \uB9CE\uC774\
  \ \uAC1C\uC120\uB418\uC5C8\uC2B5\uB2C8\uB2E4. \uB610\uD55C, `+` \uC5F0\uC0B0\uC790\
  \uB97C \uC0AC\uC6A9\uD558\uB294 \uAC83 \uC678\uC5D0\uB3C4 `concat()` \uBA54\uC18C\
  \uB4DC\uB098 `sprintf()` \uD568\uC218\uB97C \uC0AC\uC6A9\uD558\uC5EC \uBB38\uC790\
  \uC5F4\uC744 \uD569\uCE60 \uC218\uB3C4 \uC788\uC2B5\uB2C8\uB2E4. \uADF8\uB7EC\uB098\
  \u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:57.242623-06:00'
model: gpt-4-1106-preview
summary: "(\uBC29\uBC95) \uBB38\uC790\uC5F4 \uC5F0\uACB0\uC740 \uC544\uB450\uC774\uB178\
  \uC758 \uCD08\uAE30 \uBC84\uC804\uBD80\uD130 \uC0AC\uC6A9\uB418\uC5B4 \uC654\uC9C0\
  \uB9CC, \uB3D9\uC801 \uBA54\uBAA8\uB9AC \uAD00\uB9AC\uAC00 \uB9CE\uC774 \uAC1C\uC120\
  \uB418\uC5C8\uC2B5\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uC5F0\uACB0\uD558\uAE30"
weight: 3
---

## How to: (방법)
```Arduino
void setup() {
  Serial.begin(9600);
  
  String part1 = "안녕, ";
  String part2 = "아두이노!";
  String combined = part1 + part2;

  Serial.println(combined);
}

void loop() {
  // Do nothing here
}
```
출력:
```
안녕, 아두이노!
```

## Deep Dive (심층 분석)
문자열 연결은 아두이노의 초기 버전부터 사용되어 왔지만, 동적 메모리 관리가 많이 개선되었습니다. 또한, `+` 연산자를 사용하는 것 외에도 `concat()` 메소드나 `sprintf()` 함수를 사용하여 문자열을 합칠 수도 있습니다. 그러나 `String` 객체를 사용하면 동적 메모리가 할당되기 때문에 메모리가 조각화될 위험이 있습니다. 이를 피하기 위해, 메모리 사용을 더 잘 제어할 수 있는 `strcat()` 함수를 사용한 문자 배열(char array)을 활용할 수도 있습니다.

## See Also (참고하기)
- [Arduino Reference: String Object](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [Arduino Forum discussion on Strings](https://forum.arduino.cc/t/an-introduction-to-string-handling-in-arduino/583891)
- [Arduino Memory Management](https://www.arduino.cc/en/Tutorial/Foundations/Memory)
