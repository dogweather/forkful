---
changelog:
- 2024-01-28, dogweather, reviewed
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 03:00:40.517504-07:00
description: "HTML \uD30C\uC2F1\uC740 HTML \uBB38\uC11C\uC5D0\uC11C \uB370\uC774\uD130\
  \uB97C \uCD94\uCD9C\uD558\uB294 \uAC83\uC744 \uC758\uBBF8\uD569\uB2C8\uB2E4. \uD504\
  \uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC6F9 \uCF58\uD150\uCE20\uC640 \uC0C1\uD638\
  \ \uC791\uC6A9\uD558\uAC70\uB098 \uC870\uC791, \uC790\uB3D9 \uB370\uC774\uD130 \uCD94\
  \uCD9C \uB610\uB294 \uC6F9 \uC2A4\uD06C\uB798\uD551 \uBAA9\uC801\uC73C\uB85C \uC774\
  \uB97C \uC218\uD589\uD569\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.787646-06:00'
model: gpt-4-0125-preview
summary: "HTML \uD30C\uC2F1\uC740 HTML \uBB38\uC11C\uC5D0\uC11C \uB370\uC774\uD130\
  \uB97C \uCD94\uCD9C\uD558\uB294 \uAC83\uC744 \uC758\uBBF8\uD569\uB2C8\uB2E4. \uD504\
  \uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC6F9 \uCF58\uD150\uCE20\uC640 \uC0C1\uD638\
  \ \uC791\uC6A9\uD558\uAC70\uB098 \uC870\uC791, \uC790\uB3D9 \uB370\uC774\uD130 \uCD94\
  \uCD9C \uB610\uB294 \uC6F9 \uC2A4\uD06C\uB798\uD551 \uBAA9\uC801\uC73C\uB85C \uC774\
  \uB97C \uC218\uD589\uD569\uB2C8\uB2E4."
title: "HTML \uD30C\uC2F1"
weight: 43
---

## 무엇과 왜?
HTML 파싱은 HTML 문서에서 데이터를 추출하는 것을 의미합니다. 프로그래머들은 웹 콘텐츠와 상호 작용하거나 조작, 자동 데이터 추출 또는 웹 스크래핑 목적으로 이를 수행합니다.

## 방법:
자바스크립트에서 `DOMParser` API를 사용해서 HTML을 파싱해봅시다.

```Javascript
const parser = new DOMParser();
const htmlString = `<p>Hello, world!</p>`;
const doc = parser.parseFromString(htmlString, 'text/html');
console.log(doc.body.textContent); // 출력: Hello, world!
```

이제 클래스가 있는 특정 요소를 가져와 보겠습니다:

```Javascript
const htmlString = `<div><p class="greeting">Hello, again!</p></div>`;
const doc = parser.parseFromString(htmlString, 'text/html');
const greeting = doc.querySelector('.greeting').textContent;
console.log(greeting); // 출력: Hello, again!
```

## 심층 탐구
HTML 파싱은 웹만큼이나 오래되었습니다. 처음에는 브라우저가 웹 페이지를 표시하기 위해 HTML을 파싱하는 것이었습니다. 시간이 지나면서 프로그래머들은 이 과정에 접근하고자 했고, 이로 인해 `DOMParser`와 같은 API가 생겨났습니다.

대안이 있나요? 물론입니다. 우리는 `jQuery`나 파이썬용 `BeautifulSoup` 같은 라이브러리가 있습니다. 하지만 자바스크립트의 네이티브 `DOMParser`는 빠르고 내장되어 있어, 추가 라이브러리가 필요 없습니다.

실행 측면에서 볼 때, `DOMParser`로 HTML을 파싱하면 `Document` 객체를 생성합니다. 이를 HTML의 계층적 모델로 생각할 수 있습니다. 한 번 가지게 되면, 일반 웹 페이지의 DOM처럼 이를 탐색하고 조작할 수 있습니다.

여기서 중요한 점—파싱은 잘못된 HTML에서 문제를 일으킬 수 있습니다. 브라우저는 관대하지만, `DOMParser`는 그렇지 않을 수 있습니다. 따라서 복잡한 작업이나 지저분한 HTML의 경우, 타사 라이브러리가 더 나은 정리 작업을 수행할 수 있습니다.

## 참고 자료
- `DOMParser` API에 관한 MDN 웹 문서: [MDN DOMParser](https://developer.mozilla.org/en-US/docs/Web/API/DOMParser)
- jQuery의 파싱 기능: [jQuery.parseHTML()](https://api.jquery.com/jquery.parsehtml/)
- 서버용 코어 jQuery의 빠르고 유연하며 간결한 구현인 Cheerio: [Cheerio.js](https://cheerio.js.org/)
- JS가 아닌 파싱을 위해: 파이썬의 BeautifulSoup 라이브러리: [Beautiful Soup](https://www.crummy.com/software/BeautifulSoup/)
