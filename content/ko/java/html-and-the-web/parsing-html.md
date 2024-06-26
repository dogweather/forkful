---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:36.830266-07:00
description: "\uBC29\uBC95: \uC2E4\uC81C HTML \uC791\uC5C5\uC5D0 \uC720\uC6A9\uD55C\
  \ \uB77C\uC774\uBE0C\uB7EC\uB9AC\uC778 Jsoup\uC744 \uC0AC\uC6A9\uD574 \uBD05\uC2DC\
  \uB2E4. \uBA3C\uC800, \uC758\uC874\uC131\uC744 \uCD94\uAC00\uD558\uC2ED\uC2DC\uC624\
  ."
lastmod: '2024-03-13T22:44:55.045078-06:00'
model: gpt-4-0125-preview
summary: "\uC2E4\uC81C HTML \uC791\uC5C5\uC5D0 \uC720\uC6A9\uD55C \uB77C\uC774\uBE0C\
  \uB7EC\uB9AC\uC778 Jsoup\uC744 \uC0AC\uC6A9\uD574 \uBD05\uC2DC\uB2E4."
title: "HTML \uD30C\uC2F1"
weight: 43
---

## 방법:
실제 HTML 작업에 유용한 라이브러리인 Jsoup을 사용해 봅시다. 먼저, 의존성을 추가하십시오:

```xml
<dependency>
    <groupId>org.jsoup</groupId>
    <artifactId>jsoup</artifactId>
    <version>1.15.2</version>
</dependency>
```

이제 재미있는 부분으로 넘어갑니다. 웹페이지의 타이틀을 가져와서 출력하는 방법은 다음과 같습니다:

```java
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;

public class HtmlParser {
    public static void main(String[] args) throws IOException {
        String url = "http://example.com";
        Document doc = Jsoup.connect(url).get();
        String title = doc.title();
        System.out.println("Title: " + title);
    }
}
```

출력:

```
Title: Example Domain
```

모든 링크를 추출하는 것은 어떨까요?

```java
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

// ... main 메소드나 다른 메소드 안에서
Elements links = doc.select("a[href]");
for (Element link : links) {
    System.out.println("Link: " + link.attr("href"));
}
```

## 심층 분석
예전에는 HTML을 정규 표현식 패턴으로 다루었는데, 이는 복잡한 문서에 대해 오류가 잦고 악몽 같은 방법이었습니다. 2000년대 후반에 Jsoup이 등장하면서, Java로 HTML을 파싱, 순회, 조작하기 위한 jQuery와 같은 인터페이스를 제공했습니다.

Jsoup이 유일한 선택은 아닙니다. 자바스크립트 지원이 포함된 전체적인 웹 앱 테스팅을 위해 HtmlUnit이 있지만, 더 무겁고 복잡합니다. 가벼운 작업을 위해서는 URL 추출에 훌륭한 Apache Commons Validator가 있습니다.

내부적으로, Jsoup은 대량의 문서를 메모리 내에 트리로 모델링하는 DOM 파서를 사용합니다. 이 방식은 HTML 구조를 선택하고 탐색하는 것을 매우 쉽게 만듭니다. 또한, 부실한 HTML에 대해 관대하게 처리하여, 문제를 즉석에서 수정함으로써 견고한 파싱을 보장합니다.

스크랩할 때는 항상 사이트의 `robots.txt`와 서비스 약관을 확인하여 법적 문제나 IP 차단을 피하십시오.

## 참고 자료
- Jsoup 공식 문서: https://jsoup.org/
- HtmlUnit: http://htmlunit.sourceforge.net/
- Apache Commons Validator: https://commons.apache.org/proper/commons-validator/
