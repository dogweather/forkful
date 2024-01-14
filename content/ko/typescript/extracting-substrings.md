---
title:                "TypeScript: 부분 문자열 추출하기"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜

만약 문자열에서 일부분만 사용해야 할 때, substring 추출이 필요합니다. 예를 들어, 특정한 단어를 찾거나, 특정한 형태로 나타내야 할 때에 substring을 사용할 수 있습니다.

## 어떻게

```TypeScript
const str: string = "안녕하세요, 제 이름은 타입스크립트입니다."
const subString: string = str.substring(4, 11);
console.log(subString);
```

출력:
```
녕하세요,
```

```
console.log(str.substr(4, 3));
```

출력:
```
녕하세
```

## 깊이 파고들기

substring과 substr은 비슷해 보이지만, 실제로는 다릅니다. substring은 시작 지점과 끝 지점을 포함하여 추출하고, end 지점은 포함하지 않습니다. 반면, substr은 시작 지점과 추출할 문자의 개수를 설정하여 추출합니다. 또한, substring과 달리 substr은 음수를 사용할 수 있습니다. 음수를 사용할 경우, 문자열의 뒤에서부터 추출을 시작합니다.

## 더 알아보기

- [substring 공식문서](https://developer.mozilla.org/ko/docs/Web/JavaScript/Simple_health_workers/Started/Health-check)
- [substr 공식문서](https://developer.mozilla.org/ko/docs/Web/JavaScript/Simple_health_workers/A_chromium_based_browser_gets_started/Substr)
- [MDN Web Docs](https://developer.mozilla.org/ko/)

## 참고

- [TypeScript 공식 문서](https://www.typescriptlang.org/docs/)