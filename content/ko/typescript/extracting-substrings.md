---
title:                "부분 문자열 추출"
date:                  2024-01-20T17:46:31.157352-07:00
model:                 gpt-4-1106-preview
simple_title:         "부분 문자열 추출"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
문자열에서 특정 부분을 추출하는 것입니다. 데이터 가공이나 특정 정보를 얻기 위해서 프로그래머들이 사용합니다.

## How to: (어떻게 하나요?)
```TypeScript
let fullString: string = "Hello, TypeScript users!";
let substring: string = fullString.substring(7, 18);
console.log(substring); // Outputs: TypeScript
```
`substring` 메서드로 시작 인덱스와 끝 인덱스를 정해 문자열 부분을 추출할 수 있습니다.

## Deep Dive (심층 분석)
과거에는 `substr`이나 `slice`와 같은 메서드도 사용되었습니다. `substring`과 `slice`는 거의 비슷하지만, 음수 인덱스를 다루는 방법에서 차이가 있습니다. `substring`은 음수를 0으로 취급하고, `slice`는 음수를 문자열의 끝에서부터의 위치로 해석합니다. TypeScript는 JavaScript와 호환되므로 JavaScript의 문자열 메서드를 그대로 이용합니다. 성능 면에서는 이러한 메서드들의 차이는 미미하므로 상황에 맞게 선택해서 사용하면 됩니다.

## See Also (추가 정보)
- MDN Web Docs - String.prototype.substring(): https://developer.mozilla.org/docs/Web/JavaScript/Reference/Global_Objects/String/substring
- String.prototype.slice(): https://developer.mozilla.org/docs/Web/JavaScript/Reference/Global_Objects/String/slice
- TypeScript Handbook: https://www.typescriptlang.org/docs/handbook/intro.html
