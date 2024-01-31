---
title:                "HTML 파싱"
date:                  2024-01-20T15:32:03.529806-07:00
html_title:           "Arduino: HTML 파싱"
simple_title:         "HTML 파싱"

category:             "Java"
tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (무엇인가 & 왜 사용하는가?)
HTML 파싱은 HTML 문서에서 데이터를 추출하는 과정입니다. 웹 스크래핑, 데이터 마이닝 등에서 필요하며, 웹에서 정보를 자동으로 수집하고자 할 때 사용합니다.

## How to (방법)
자바에서 HTML 파싱을 위해 Jsoup 라이브러리를 사용할 수 있습니다. Jsoup을 활용하면 쉽게 HTML 요소에 접근하고 조작할 수 있습니다. 예시 코드를 확인해 보세요.

```java
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

public class HtmlParserExample {
    public static void main(String[] args) throws IOException {
        String htmlString = "<html><head><title>Sample Title</title></head>"
            + "<body><p>Parsed HTML into a doc.</p></body></html>";
        
        Document doc = Jsoup.parse(htmlString);
        String title = doc.title();  
        Elements paragraphs = doc.select("p");

        System.out.println("Title: " + title);
        for (Element paragraph : paragraphs) {
            System.out.println("Paragraph: " + paragraph.text());
        }
    }
}
```

출력 예시:

```
Title: Sample Title
Paragraph: Parsed HTML into a doc.
```

## Deep Dive (심층 분석)
HTML 파싱 기술은 2000년대 초반 웹의 급성장과 더불어 중요해졌습니다. Jsoup 외에도 HTMLUnit, JsoupXML과 같은 대안이 있지만 Jsoup는 직관적인 API 때문에 많이 선호됩니다. 이 라이브러리는 내부적으로 HTML 문서를 DOM 구조로 변환하여 쉽게 탐색하고 조작할 수 있도록 해줍니다. 

## See Also (참고 자료)
- Jsoup 공식 문서: https://jsoup.org/
- HTMLUnit 공식 페이지: http://htmlunit.sourceforge.net/
- W3C HTML 파싱 가이드라인: https://www.w3.org/TR/html5/syntax.html#parsing
