---
title:                "HTML 파싱"
html_title:           "Arduino: HTML 파싱"
simple_title:         "HTML 파싱"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/parsing-html.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하는가?

HTML 분석을 통해, 프로그래머들은 웹페이지의 구조 및 데이터를 추출하고 활용할 수 있습니다. 이를 통해 얻은 데이터는 다양하게 재사용되거나 분석에 사용됩니다.

## 어떻게 사용하는가:

```Gleam
fn parse_html(html: String) -> Result(tree: HtmlTree, err: HtmlParseError) {
let parser = HtmlParser::new();
let document = parser.parse(&html);
match document {
        Ok(tree) -> {
          Ok(tree)
        }
        Err(err) -> {
          Error(err)
        }
    }
}

let html = "<html><body><h1>Hello, Gleam!</h1></body></html>";
let result = parse_html(html);
  match result {
    Ok(tree) -> {
     println!("{:?}", tree);
    }
    Err(err) -> {
     println!("Error: {:?}", err);
    }
}
```

위의 예제에서는 `HtmlParser::new()`, `parser.parse()` 두 가지 주요 메서드를 사용하여 HTML을 분석합니다. 예상되는 결과는 웹페이지 구조에 따라 다릅니다.

## 깊숙히 알아보기 

HTML을 분석하는 것은 웹 크롤링의 핵심적인 역할입니다. 웹페이지에서 데이터를 추출하고 저장하는 초기 방법 중 하나였습니다. 대안으로는 API (Application Programming Interface)를 사용하는 것이 있습니다. API는 웹사이트에서 직접 데이터를 요청하기 위한 방법을 제공하지만, 모든 웹사이트가 API를 제공하는 것은 아닙니다. 따라서, HTML 분석은 여전히 유용합니다. HTML 분석에 관한 Gleam의 구현 상세 내용은 [Gleam의 공식 문서](https://gleam.run/book/)를 참조하십시오.

## 참고 자료

1. Gleam 공식 문서: https://gleam.run/book/
2. HTML Parsing에 대한 자세한 아티클: https://www.freecodecamp.org/news/html-parsing/
3. Python을 활용한 HTML Parsing 예제: https://realpython.com/python-html-parser/