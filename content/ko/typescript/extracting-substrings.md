---
title:    "TypeScript: The translated title is 부분 문자열 추출하기."
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## 왜

파서와 다른 문자열 관련 함수와 비교해 볼 때, 문자열의 부분을 추출하는 것은 유용한 작업 중 하나입니다. 부분 문자열이란 원하는 문자열의 일부분을 가리키며, 문자열을 조작하고 분석하는데 유용합니다.

## 사용 방법

### 시작 위치부터 문자열 추출하기

```TypeScript
let myString: string = "나는 TypeScript를 배우고 있어.";
let substring: string = myString.substr(8);
console.log(substring); // "TypeScript를 배우고 있어."
```

위의 코드에서는 `substr()` 함수를 사용하여 시작 위치부터 문자열의 마지막까지를 추출할 수 있습니다. 첫 번째 인자는 시작 위치를 나타내며, 두 번째 인자는 선택적으로 추출할 문자열의 길이를 지정할 수 있습니다.

### 시작 위치와 길이로 문자열 추출하기

```TypeScript
let myString: string = "나는 TypeScript를 배우고 있어.";
let substring: string = myString.substr(8, 11);
console.log(substring); // "TypeScript"
```

두 번째 인자를 추가하여 추출할 문자열의 길이를 지정할 수도 있습니다. 위의 코드에서는 8번째 글자부터 11개의 글자를 추출하였기 때문에 "TypeScript"가 출력됩니다.

### 시작 위치와 끝 위치로 문자열 추출하기

```TypeScript
let myString: string = "나는 TypeScript를 배우고 있어.";
let substring: string = myString.substring(8, 19);
console.log(substring); // "TypeScript"
```

또 다른 방법으로 `substring()` 함수를 사용하여 시작 위치와 끝 위치를 지정하여 문자열을 추출할 수 있습니다. 첫 번째 인자는 시작 위치를, 두 번째 인자는 끝 위치를 나타냅니다. 끝 위치는 추출되는 문자열에 포함되지 않기 때문에 19번째 글자는 포함되지 않았습니다.

## 깊게 파헤치기

추출한 부분 문자열을 활용하여 다양한 작업을 할 수 있습니다. 예를 들어, 정규표현식과 함께 사용하여 원하는 형식의 문자열만 추출하는 작업이 가능합니다. 또는 추출한 부분 문자열을 다른 변수에 할당하여 더 복잡한 문자열 조작을 할 수도 있습니다.

## 참고자료

- [MDN - substring()](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [MDN - substr()](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/substr)