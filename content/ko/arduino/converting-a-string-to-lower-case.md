---
title:    "Arduino: 문자열 소문자로 변환하기"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# 왜

문자열을 소문자로 변환하는 것의 중요성을 알아보십시오. 만약 당신이 프로그래밍을 할 때, 대문자와 소문자를 구분하는 것은 매우 중요합니다. 때로는, 입력 데이터로부터 소문자로 변환된 문자열을 얻는 것이 필요할 때가 있기 때문입니다. 그래서 우리는 Arduino에서 문자열을 소문자로 변환하는 방법을 배워보겠습니다.

# 하나:

아래 코드를 확인하십시오.

```Arduino
String str = "HeLlO wOrLd!";
str.toLowerCase();
Serial.println(str);
```

출력: hello world!

위의 예제에서 우리는 문자열 "HeLlO wOrLd!"를 입력하고, `toLowerCase()` 함수를 호출하여 소문자로 변환하였습니다. 그리고 필터된 결과를 직접 출력하였습니다. 이것은 아주 간단하고 효율적인 방법입니다!

# 깊은 고민 (Deep Dive):

`toLowerCase()` 함수는 문자열로 바로 적용될 수 있는 유용한 함수입니다. 하지만 이 함수가 어떻게 작동하는지에 대해서 조금 더 깊이 알아보도록 하겠습니다.

C++ 프로그래밍 언어에서 소문자로 변환하는데 사용될 수 있는 `toupper()`이라는 함수가 있습니다. Arduino IDE는 C++ 언어로 코드를 작성하기 때문에, `toLowerCase()` 함수도 실제로는 `toupper()` 함수를 호출하여 결과를 반환합니다. 따라서 `toLowerCase()` 함수는 다음과 같이 작동합니다.

1. 우선 문자를 읽습니다.
2. 그 문자가 소문자인지 아닌지를 판별합니다.
3. 소문자일 경우, 그대로 반환합니다.
4. 대문자일 경우, `toupper()` 함수를 사용하여 소문자로 변환합니다.
5. 변환된 문자를 결과 문자열에 추가합니다.
6. 모든 문자를 확인하고 나면, 최종 결과를 반환합니다.

따라서 `toLowerCase()` 함수는 내부적으로 많은 작업을 실행하지만, 우리는 간단하게 사용할 수 있습니다. 좋은 함수는 복잡한 로직을 실제 사용자에게 감춰주는 것이기 때문입니다.

# 참고 자료 (See Also):

- [String.toLowerCase() - Arduino Reference](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/tolowercase/)
- [toupper(), tolower() - C++ Reference](http://www.cplusplus.com/reference/cctype/toupper/)
- [C++ Programming Language - Wikipedia](https://en.wikipedia.org/wiki/C%2B%2B)

결론을 짓지 않고, 우리는 이제 Arduino에서 문자열을 소문자로 변환하는 방법을 알게 되었습니다. 이 글이 여러분의 프로젝트나 코딩 공부에 도움이 되었기를 바랍니다!