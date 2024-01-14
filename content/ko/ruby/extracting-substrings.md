---
title:    "Ruby: 부분 문자열 추출"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## 왜

자바스크립트 프로그래밍을 배우는 많은 사람들이 들어 자바스크립트에서 하위 문자열을 추출하는 방법을 배우려고 할 수 있습니다. 하위 문자열을 추출하는 것은 문자열을 조작하고 다양한 작업을 수행하는 데 도움이되기 때문입니다.

## 하는 방법

자바스크립트에서 하위 문자열을 추출하는 방법은 간단합니다. 문자열 객체의 `substring()` 메서드를 사용하면 됩니다. 아래는 코드 예제와 함께 하위 문자열을 추출하는 방법을 보여줍니다.

```Ruby
# 문자열 생성
string = "안녕하세요! 반가워요!"

# `substring()` 메서드를 사용하여 첫 번째 문자부터 세 번째 문자까지 추출
substring = string.substring(0,2)

puts substring
# 출력 결과: 안녕

# `substring()` 메서드를 사용하여 두 번째 문자부터 마지막 문자까지 추출
substring = string.substring(1)

puts substring
# 출력 결과: 녕하세요! 반가워요!
```

위의 코드 예제에서는 `substring()` 메서드의 매개변수를 조정하여 원하는 부분의 하위 문자열을 추출할 수 있습니다. 또한 `substring()` 메서드는 원본 문자열을 변경하지 않고 새로운 하위 문자열을 반환하기 때문에 유용하게 사용할 수 있습니다.

## 깊게 파헤치기

자바스크립트에서 문자열을 조작하는 여러 가지 방법 중 하나는 하위 문자열의 추출입니다. 하위 문자열을 추출하는 것은 `substring()` 메서드를 사용하여 쉽게 할 수 있습니다. 그러나 자바스크립트에는 다른 문자열 조작 메서드들도 있으니, 관심있는 분들은 더 많은 공부를 해보시기 바랍니다.

## 관련 자료

- [JavaScript Strings Tutorial](https://www.w3schools.com/js/js_strings.asp)
- [MDN Web Docs: String substring() method](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)