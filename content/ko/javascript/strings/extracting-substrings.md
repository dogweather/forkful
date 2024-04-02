---
date: 2024-01-20 17:45:59.706072-07:00
description: "JavaScript\uC5D0\uC11C \uBB38\uC790\uC5F4\uC758 \uC77C\uBD80\uB97C \uCD94\
  \uCD9C\uD558\uB294 \uAC83\uC740 \uD2B9\uC815 \uBD80\uBD84\uC758 \uB370\uC774\uD130\
  \uB97C \uC5BB\uAE30 \uC704\uD568\uC785\uB2C8\uB2E4. \uAC80\uC0C9, \uB370\uC774\uD130\
  \ \uC815\uC81C \uB610\uB294 UI \uD45C\uC2DC\uC640 \uAC19\uC740 \uB9CE\uC740 \uC791\
  \uC5C5\uC5D0 \uD544\uC218\uC801\uC785\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.775550-06:00'
model: gpt-4-1106-preview
summary: "JavaScript\uC5D0\uC11C \uBB38\uC790\uC5F4\uC758 \uC77C\uBD80\uB97C \uCD94\
  \uCD9C\uD558\uB294 \uAC83\uC740 \uD2B9\uC815 \uBD80\uBD84\uC758 \uB370\uC774\uD130\
  \uB97C \uC5BB\uAE30 \uC704\uD568\uC785\uB2C8\uB2E4. \uAC80\uC0C9, \uB370\uC774\uD130\
  \ \uC815\uC81C \uB610\uB294 UI \uD45C\uC2DC\uC640 \uAC19\uC740 \uB9CE\uC740 \uC791\
  \uC5C5\uC5D0 \uD544\uC218\uC801\uC785\uB2C8\uB2E4."
title: "\uBD80\uBD84 \uBB38\uC790\uC5F4 \uCD94\uCD9C"
weight: 6
---

## What & Why? (무엇과 왜?)
JavaScript에서 문자열의 일부를 추출하는 것은 특정 부분의 데이터를 얻기 위함입니다. 검색, 데이터 정제 또는 UI 표시와 같은 많은 작업에 필수적입니다.

## How to (방법):
```Javascript
// substring() 사용
let text = "Hello, JavaScript!";
let part = text.substring(7, 19); // "JavaScript!"
console.log(part);

// slice() 사용
part = text.slice(7, 19); // "JavaScript!"
console.log(part);

// substr() 사용 (비권장)
part = text.substr(7, 12); // "JavaScript!"
console.log(part);
```
출력:
```
JavaScript!
JavaScript!
JavaScript!
```

## Deep Dive (심층 분석):
substring, slice, 그리고 substr은 모두 JavaScript에서 문자열을 추출하는 방법입니다. substring과 slice는 매우 유사하지만 매개 변수에 음수가 들어갈 경우의 행동이 다릅니다. substr은 현재는 권장되지 않으며 추후 표준에서 제거될 예정입니다. 이러한 함수는 문자열 작업을 쉽고 효율적으로 만드는 중요한 도구입니다.

## See Also (참조):
- MDN Web Docs substring(): https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring
- MDN Web Docs slice(): https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice
- MDN Web Docs substr(): https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substr (단, 이 링크는 이 함수가 더 이상 권장되지 않음을 알립니다)
