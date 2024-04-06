---
date: 2024-01-20 17:45:59.706072-07:00
description: "How to (\uBC29\uBC95): substring, slice, \uADF8\uB9AC\uACE0 substr\uC740\
  \ \uBAA8\uB450 JavaScript\uC5D0\uC11C \uBB38\uC790\uC5F4\uC744 \uCD94\uCD9C\uD558\
  \uB294 \uBC29\uBC95\uC785\uB2C8\uB2E4. substring\uACFC slice\uB294 \uB9E4\uC6B0\
  \ \uC720\uC0AC\uD558\uC9C0\uB9CC \uB9E4\uAC1C \uBCC0\uC218\uC5D0 \uC74C\uC218\uAC00\
  \ \uB4E4\uC5B4\uAC08 \uACBD\uC6B0\uC758 \uD589\uB3D9\uC774 \uB2E4\uB985\uB2C8\uB2E4\
  . substr\uC740 \uD604\uC7AC\uB294 \uAD8C\uC7A5\uB418\uC9C0 \uC54A\uC73C\uBA70\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:57.381792-06:00'
model: gpt-4-1106-preview
summary: "substring, slice, \uADF8\uB9AC\uACE0 substr\uC740 \uBAA8\uB450 JavaScript\uC5D0\
  \uC11C \uBB38\uC790\uC5F4\uC744 \uCD94\uCD9C\uD558\uB294 \uBC29\uBC95\uC785\uB2C8\
  \uB2E4."
title: "\uBD80\uBD84 \uBB38\uC790\uC5F4 \uCD94\uCD9C"
weight: 6
---

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
