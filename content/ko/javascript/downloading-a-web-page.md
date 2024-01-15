---
title:                "웹 페이지 다운로드"
html_title:           "Javascript: 웹 페이지 다운로드"
simple_title:         "웹 페이지 다운로드"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 왜
웹 페이지를 다운로드하는 것이 왜 중요한지 궁금하신가요? 웹 페이지를 다운로드하면 오프라인에서도 웹 사이트의 내용을 쉽게 볼 수 있기 때문입니다. 또한, 해당 웹 페이지에 저장된 정보를 바탕으로 데이터 분석, 스크래핑 등 다양한 작업을 할 수 있습니다.

## 어떻게 다운로드할까요?
웹 페이지를 다운로드 하는 방법은 여러 가지가 있지만, 우리는 Javascript를 이용하는 방법에 대해 알아보겠습니다. 먼저, URL을 사용하여 웹 페이지에 접근합니다. 그리고 다운로드할 내용을 선택하고 다운로드 방식을 정하면 됩니다. 다음은 간단한 코드 예시입니다.

```Javascript
// 필요한 모듈 import
const axios = require('axios');
const fs = require('fs');

// URL 정의
const url = "https://example.com";

// axios를 사용하여 웹 페이지 다운로드
axios.get(url)
    .then((response) => {
        // 다운로드한 웹 페이지의 내용을 파일로 저장
        fs.writeFile('webpage.html', response.data, (err) => {
            if (err) throw err;
            console.log("웹 페이지 다운로드 완료!");
        });
    })
    .catch((error) => {
        console.log(error);
    })
```

위 코드는 Node.js 환경에서 실행할 수 있으며, 다운로드한 웹 페이지를 파일로 저장하고 있습니다. 해당 파일을 열어보면 웹 페이지의 내용이 그대로 저장되어 있을 것입니다.

## 깊이 들어가기
웹 페이지를 다운로드하는 것은 보다 복잡한 작업을 위한 출발점일 뿐입니다. Javascript를 이용하여 웹 페이지를 분석하거나 스크래핑하는 작업도 가능합니다. 또한, 다운로드한 웹 페이지 내에서 자신이 원하는 정보를 추출하는 작업도 가능합니다. 예를 들어, 위에서 다운로드한 웹 페이지의 내용을 문자열로 변환하여 특정 단어가 포함되어 있는지 확인하고, 포함되어 있다면 해당 단어를 추출하는 작업을 할 수 있습니다.

## 더 알아보기
다운로드한 웹 페이지를 분석하거나 스크래핑하는 작업을 더 알아보려면 아래 링크들을 참고해보세요.

- [Cheerio를 이용한 웹 스크래핑](https://cheerio.js.org/)
- [Puppeteer를 이용한 웹 페이지 자동화](https://pptr.dev/)
- [Node.js 라이브러리 모음](https://www.npmjs.com/)