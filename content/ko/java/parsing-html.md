---
title:                "HTML 파싱"
html_title:           "Fish Shell: HTML 파싱"
simple_title:         "HTML 파싱"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/parsing-html.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하는가?
HTML 파싱이란 프로그래머가 조작하거나 분석할 수 있게 HTML 문서를 데이터 구조로 변환하는 방법입니다. 이 과정은 웹 크롤링, 웹 페이지 내용 분석 및 수정 등 다양한 프로그래밍 작업에서 필수적인 단계입니다.

## 이렇게 해보세요:
자바에서 HTML을 파싱하기 위해世界적으로 널리 사용되는 라이브러리 중 하나인 JSoup을 사용합니다. JSoup은 웹 페이지에서 데이터를 스크립트화하고 추출하는 데 사용되며, 사용하기 아주 편리한 API를 제공합니다.

사용 사례로 아래 HTML 문서에서 모든 링크를 추출해보겠습니다.


```Java
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

public class Main {
    public static void main(String[] args) throws Exception {
        String html = "<html><head><title>초기 웹페이지</title></head>"
                   + "<body><p>아래는 직접 입력한 HTML 링크입니다.</p>"
                   + "<a href=\"http://example.com\">자세히 보기</a>"
                   + "</body></html>";
        Document doc = Jsoup.parse(html);
        Elements links = doc.select("a");
        for (Element link : links) {
            System.out.println("링크 : " + link.attr("href"));
            System.out.println("텍스트: " + link.text());
        }
    }
}
```

위 코드를 실행하면 다음과 같이 출력됩니다:

```shell
링크 : http://example.com
텍스트: 자세히 보기
```

## 깊이 들여다보기
사실 HTML 파싱과 이를 가능하게 하는 기술은 웹의 초기 시작부터 존재했습니다. 최근에는 HTML5와 EOF를 지원하며 웹 페이지의 동적 콘텐츠를 보다 잘 처리하는 JSoup 같은 최신 라이브러리가 등장했습니다.

HTML 파싱의 대안으로는 정규 표현식(regular expression), XML 파서 등이 있지만 이들은 JSoup처럼 사용하기 쉽지 않거나 HTML에 특화되지 않았을 수 있습니다.

구현 세부사항으로는, JSoup은 내부적으로 SAX (Simple API for XML) 파싱 메커니즘을 사용하여 HTML을 파싱합니다. 이를 통해 효율적으로 대용량 HTML 문서를 처리할 수 있게 됩니다.

## 참고하기
- JSoup 공식 문서: https://jsoup.org/
- HTML 파싱 관련 Java API 가이드: https://docs.oracle.com/javase/tutorial/jaxp/dom/readingXML.html