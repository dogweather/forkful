---
title:                "HTML 구문 분석"
html_title:           "Java: HTML 구문 분석"
simple_title:         "HTML 구문 분석"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/parsing-html.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
HTML 파싱이 무엇인지를 알고싶다면 이 게시글을 읽어보세요! 파싱은 HTML 문서를 읽고 분석하는 것을 말합니다. 프로그래머들이 이를 하는 이유는 웹 데이터를 가져오거나 웹사이트를 구조화하기 위해서입니다.

## 방법:
아래에 ```Java ... ``` 코드 블록으로 예제와 출력을 보여드립니다. 이를 따라해보세요!

- 일반적인 방법:
```Java
Document doc = Jsoup.connect(url).get();
String title = doc.title();
System.out.println(title);
```
```
결과: Hello World
```

- CSS 선택자를 사용하여 데이터 추출하기:
```Java
Elements links = doc.select("a[href]");
for(Element link : links) {
	System.out.println(link.attr("href"));
}
```
```
결과: https://www.example.com
```

## 깊이있게:
이제 많은 웹 페이지들이 HTML 대신 JavaScript를 사용합니다. 그래서 파싱 시 어려움이 있을 수 있습니다. 하지만 우리는 이를 해결할 수 있는 다른 라이브러리들을 사용할 수 있습니다. 예를 들어, 개발자들이 자주 사용하는 Jsoup 외에도 다른 라이브러리들이 있습니다. 또한, 파싱 작업을 할 때 이해할 수 있는 개념들이 많이 존재합니다.

## 참고:
- [Jsoup](https://jsoup.org/)
- [Apache HttpClient](https://hc.apache.org/httpclient-legacy/index.html)
- [HtmlUnit](https://htmlunit.sourceforge.io/)

이제 HTML 파싱에 대해 더 많이 알았기를 바랍니다. 이를 바탕으로 여러분만의 웹 데이터 추출 프로그램을 만들어보세요!