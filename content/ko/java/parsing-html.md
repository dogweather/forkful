---
title:                "HTML 파싱"
html_title:           "Java: HTML 파싱"
simple_title:         "HTML 파싱"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/parsing-html.md"
---

{{< edit_this_page >}}

## 왜
HTML 파싱을 할 이유는 데이터를 추출하거나 웹사이트에서 필요한 정보를 가져오기 위해서입니다.

## 이렇게 하세요
```Java
// Jsoup 라이브러리를 사용해서 HTML을 파싱하는 예제
String url = "https://www.example.com";
Document doc = Jsoup.connect(url).get();
String title = doc.select("title").text();

System.out.println(title); // 결과: Example Domain
```

```Java
// Regex를 사용해서 HTML에서 특정 내용을 추출하는 예제
String html = "<p>Welcome to <b>Java</b></p>";
Pattern pattern = Pattern.compile("<b>(.*?)</b>");
Matcher matcher = pattern.matcher(html);

if (matcher.find()) {
  System.out.println(matcher.group(1)); // 결과: Java
}
```

## 더 깊이 알아보기
HTML 파싱 과정에서는 주로 자주 사용되는 라이브러리인 Jsoup이나 정규식을 사용합니다. HTML은 트리 구조로 되어 있기 때문에 파싱할 때는 요소의 계층을 잘 파악하고 선택할 수 있어야 합니다.

## 더 알아볼 만한 자료
- [Jsoup 공식 문서](https://jsoup.org/)
- [정규식에 대한 자세한 설명](https://www.regular-expressions.info/index.html)