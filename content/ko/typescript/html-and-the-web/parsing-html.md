---
title:                "HTML 파싱"
aliases:
- /ko/typescript/parsing-html.md
date:                  2024-02-03T19:13:21.484658-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTML 파싱"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?

HTML 파싱은 HTML 코드를 살펴보아 정보를 찾아내거나 추출하거나 조작하는 것을 말합니다. 프로그래머들은 데이터를 스크래핑하거나, 브라우저를 자동화하는 등 웹 콘텐츠와 상호작용하기 위해 이 작업을 수행합니다.

## 방법:

시작하려면 `node-html-parser`와 같은 라이브러리를 설치하십시오. 다음은 터미널 명령입니다:

```bash
npm install node-html-parser
```

이제 TypeScript에서 기본 HTML을 파싱해 봅시다:

```typescript
import { parse } from 'node-html-parser';

const html = `<ul class="fruits">
                <li>Apple</li>
                <li>Banana</li>
              </ul>`;

const root = parse(html);
console.log(root.querySelector('.fruits').textContent);  // "Apple Banana"
```

그리고 바나나만 추출하고 싶다면:

```typescript
const bananas = root.querySelectorAll('li')[1].textContent;
console.log(bananas);  // "Banana"
```

## 심층 탐구

HTML 파싱은 새로운 것이 아니며 웹 초기부터 존재했습니다. 처음에 개발자들은 정규 표현식을 사용했을 수 있지만, 그것은 금방 복잡해졌습니다. 여기서 DOM 파서가 등장했습니다: 안정적이지만 브라우저 제한이 있습니다.

`node-html-parser`와 같은 라이브러리는 고통을 추상화해 줍니다. jQuery처럼 HTML을 쿼리 할 수 있게 하지만, Node.js로 서버 측에서 작동합니다. 이것은 빠르며, 잘못된 HTML에 대해 관대하고, DOM 친화적입니다.

`jsdom`도 있습니다, 이것은 전체 브라우저 환경을 시뮬레이션 합니다. 더 무겁지만 더 철저하며, 조작 및 상호작용을 위한 완전한 Document Object Model (DOM)을 생성합니다.

Cheerio도 잊지 마십시오. 이것은 속도와 jQuery와 유사한 문법 및 더 작은 발자국을 결합하여 두 세계 사이에서 행복하게 자리 잡고 있습니다.

## 참고

더 알고 싶으시다면 이것들을 둘러보십시오:
- [DOM 파싱 및 직렬화 W3C 사양](https://www.w3.org/TR/DOM-Parsing/)
- [GitHub에서 node-html-parser](https://github.com/taoqf/node-html-parser)
- [GitHub에서 jsdom 저장소](https://github.com/jsdom/jsdom)
- [Cheerio 웹사이트](https://cheerio.js.org/)
