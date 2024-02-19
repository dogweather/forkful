---
aliases:
- /ko/typescript/searching-and-replacing-text/
date: 2024-01-20 17:58:53.915092-07:00
description: "\uD14D\uC2A4\uD2B8 \uAC80\uC0C9 \uBC0F \uAD50\uCCB4\uB294 \uBB38\uC790\
  \uC5F4 \uB0B4\uC5D0\uC11C \uD2B9\uC815 \uD14D\uC2A4\uD2B8\uB97C \uCC3E\uC544 \uB2E4\
  \uB978 \uD14D\uC2A4\uD2B8\uB85C \uBC14\uAFB8\uB294 \uACFC\uC815\uC785\uB2C8\uB2E4\
  . \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uB370\uC774\uD130 \uC815\uC81C, \uCF54\
  \uB4DC \uB9AC\uD329\uD1A0\uB9C1, \uC0AC\uC6A9\uC790 \uC785\uB825 \uCC98\uB9AC \uC2DC\
  \ \uC774 \uAE30\uB2A5\uC744 \uD65C\uC6A9\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: 2024-02-18 23:09:05.797691
model: gpt-4-1106-preview
summary: "\uD14D\uC2A4\uD2B8 \uAC80\uC0C9 \uBC0F \uAD50\uCCB4\uB294 \uBB38\uC790\uC5F4\
  \ \uB0B4\uC5D0\uC11C \uD2B9\uC815 \uD14D\uC2A4\uD2B8\uB97C \uCC3E\uC544 \uB2E4\uB978\
  \ \uD14D\uC2A4\uD2B8\uB85C \uBC14\uAFB8\uB294 \uACFC\uC815\uC785\uB2C8\uB2E4. \uD504\
  \uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uB370\uC774\uD130 \uC815\uC81C, \uCF54\uB4DC\
  \ \uB9AC\uD329\uD1A0\uB9C1, \uC0AC\uC6A9\uC790 \uC785\uB825 \uCC98\uB9AC \uC2DC\
  \ \uC774 \uAE30\uB2A5\uC744 \uD65C\uC6A9\uD569\uB2C8\uB2E4."
title: "\uD14D\uC2A4\uD2B8 \uAC80\uC0C9 \uBC0F \uAD50\uCCB4"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
텍스트 검색 및 교체는 문자열 내에서 특정 텍스트를 찾아 다른 텍스트로 바꾸는 과정입니다. 프로그래머들은 데이터 정제, 코드 리팩토링, 사용자 입력 처리 시 이 기능을 활용합니다.

## How to: (방법)
TypeScript에서 텍스트를 검색 및 교체하는 간단한 예제코드입니다.

```TypeScript
function replaceText(input: string, search: string, replaceWith: string): string {
    return input.replace(new RegExp(search, 'g'), replaceWith);
}

// 사용 예시
const originalText = "안녕하세요, 여러분! TypeScript를 사용하여 텍스트를 교체해봅시다.";
const searchText = "TypeScript";
const newText = "JavaScript";

const updatedText = replaceText(originalText, searchText, newText);
console.log(updatedText); // "안녕하세요, 여러분! JavaScript를 사용하여 텍스트를 교체해봅시다."
```

## Deep Dive (심층 분석)
텍스트 교체는 유닉스의 초기 버전에서 사용되기 시작했고, 문자열 처리에 필수적인 역할을 해왔습니다. `String.prototype.replace`는 JavaScript와 TypeScript에서 제공되는 내장 메서드로, 정규 표현식 또는 단순 문자열 검색에 사용됩니다. 정규 표현식을 사용하면 텍스트 패턴을 더 세밀하게 일치 시킬 수 있습니다 ('g' 플래그는 "전역 검색" 을 의미하여 모든 일치 항목을 교체합니다). `String.prototype.replaceAll` 메서드도 ES2021부터 추가되어 문자열 전체를 일괄 교체할 수 있습니다. 또한, 다양한 라이브러리들이 더 복잡한 텍스트교체 기능을 제공하니 필요에 따라 찾아보시는 것도 좋습니다.

## See Also (참고자료)
- RegExp Guide: [https://developer.mozilla.org/ko/docs/Web/JavaScript/Guide/Regular_Expressions](https://developer.mozilla.org/ko/docs/Web/JavaScript/Guide/Regular_Expressions)
- TypeScript Handbook: [https://www.typescriptlang.org/docs/handbook/intro.html](https://www.typescriptlang.org/docs/handbook/intro.html)
