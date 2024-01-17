---
title:                "HTML 구문 분석"
html_title:           "Javascript: HTML 구문 분석"
simple_title:         "HTML 구문 분석"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/parsing-html.md"
---

{{< edit_this_page >}}

# 무엇 & 왜? 
HTML 파싱은 HTML 문서를 분석해서 웹 페이지의 내용을 추출하는 프로세스입니다. 프로그래머들은 이것을 사용해서 HTML 문서의 내용을 동적으로 조작하거나 수정하는 등 다양한 목적으로 활용합니다.

# 하는 방법:
```Javascript
// HTML 파싱에 필요한 라이브러리를 불러옵니다.
const parseHTML = require('htmlparser2');
// 파싱할 HTML 문서를 변수에 저장합니다.
const htmlString = '<html><body><h1>Hello World!</h1></body></html>';
// 파서 객체를 생성합니다.
const parser = new parseHTML.Parser({
  // 태그가 열릴 때 실행할 함수를 지정합니다.
  onopentag: function(name, attributes) {
      console.log(name + " 태그가 열렸습니다.");
  },
  // 태그 내부의 텍스트를 읽을 때 실행할 함수를 지정합니다.
  ontext: function(text) {
      console.log("태그 내부의 텍스트: " + text);
  },
  // 태그가 닫힐 때 실행할 함수를 지정합니다.
  onclosetag: function(name) {
      console.log(name + " 태그가 닫혔습니다.");
  }
}, {decodeEntities: true});
// 파서에 HTML 문서를 입력합니다.
parser.write(htmlString);
// 파서가 작업을 마치고 종료됩니다.
parser.end();
```
출력:
```
html 태그가 열렸습니다.
body 태그가 열렸습니다.
h1 태그가 열렸습니다.
태그 내부의 텍스트: Hello World!
h1 태그가 닫혔습니다.
body 태그가 닫혔습니다.
html 태그가 닫혔습니다.
```

# 깊이 있는 정보:
HTML 파싱은 웹의 발전과 함께 발전한 기술입니다. 초기에는 정적인 HTML 문서에서 정보를 추출하는 용도로 사용되었지만, 현재는 웹 사이트들이 동적으로 변하고 있는 시대에 맞춰서 다양한 용도로 활용됩니다. 또한 파서 라이브러리 외에도 jQuery나 Cheerio 같은 다른 라이브러리를 활용해서 HTML을 파싱할 수도 있습니다. 구현 방식은 각각의 라이브러리마다 다릅니다.

# 관련 자료:
- [HTML 파싱 튜토리얼 - MDN](https://developer.mozilla.org/en-US/docs/Web/Guide/HTML) 
- [Cheerio - Fast, flexible & lean implementation of core jQuery designed specifically for the server.](https://cheerio.js.org) 
- [htmlparser2 - A forgiving HTML/XML/RSS parser. The parser that powers wtf_wikipedia](https://github.com/fb55/htmlparser2)