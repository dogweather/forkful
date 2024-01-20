---
title:                "웹 페이지 다운로드하기"
html_title:           "Bash: 웹 페이지 다운로드하기"
simple_title:         "웹 페이지 다운로드하기"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

웹 페이지 다운로드는 인터넷에서 웹 페이지의 HTML 코드를 가져오는 작업입니다. 프로그래머들이 이 작업을 수행하는 주된 이유는 웹 페이지의 데이터를 분석하거나 저장하기 위함입니다.

## 방법:

다음은 Java에서 웹 페이지를 다운로드하는 방법에 대한 코드 예제입니다.

```Java
import java.io.*;
import java.net.URL;
import java.nio.charset.Charset;

public class WebDownloader {
    private static String readAll(Reader rd) throws IOException {
        StringBuilder sb = new StringBuilder();
        int cp;
        while ((cp = rd.read()) != -1) {
            sb.append((char) cp);
        }
        return sb.toString();
    }

    public static String downloadWebPage(String webPageUrl) throws IOException {
        URL url = new URL(webPageUrl);
        try( InputStream is = url.openStream()) {
            BufferedReader rd = new BufferedReader(new InputStreamReader(is, Charset.forName("UTF-8")));
            return readAll(rd);
        }
    }

    public static void main(String[] args) throws IOException{
         System.out.println(downloadWebPage("http://www.google.com"));
    }
}
```

이렇게 하면 출력은 다운로드된 Google 홈페이지의 HTML 코드입니다.

## Deep Dive:

웹 페이지를 다운로드하는 것은 웹 크롤링의 기본적인 요소입니다. Java가 초기에 발표되었을 때부터 이 방식이 사용되고 있습니다. 대안으로는 Jsoup이나 HtmlUnit과 같은 라이브러리를 사용하여 웹 페이지를 동적으로 파싱하고 데이터를 추출할 수 있습니다.

위의 구현에서는 매우 기본적인 스크립트를 제공했습니다만, 실제로는 훨씬 더 복잡한 기능을 구현할 수 있습니다. 예를 들어, 다운로드한 페이지를 분석하여 특정 태그 또는 데이터를 찾거나, 특정 링크를 따라 들어가서 추가 페이지를 다운로드하는 등의 작업을 수행할 수 있습니다.

## 참고 자료:

- Java 문서: [Java HttpURLConnection](https://docs.oracle.com/javase/8/docs/api/java/net/HttpURLConnection.html)
- Jsoup API: [Jsoup](https://jsoup.org/)
- HtmlUnit API: [HtmlUnit](http://htmlunit.sourceforge.net/)