---
title:                "웹 페이지 다운로드"
html_title:           "Java: 웹 페이지 다운로드"
simple_title:         "웹 페이지 다운로드"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 왜
웹 페이지를 다운로드하는 이유는 그 페이지에 포함된 정보를 읽고 이를 활용하기 위함입니다. 다양한 웹사이트에서 정보를 가져오는 것을 자동화하여 효율적으로 데이터를 이용할 수 있습니다.

## 다운로드하는 방법
웹 페이지를 다운로드하는 데에는 여러 가지 방법이 있지만, 여기서는 Java를 사용하여 가장 간단한 방법인 HTTPURLConnection 클래스를 이용하는 방법을 다루겠습니다.

```Java
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;

public class WebPageDownloader {
    public static void main(String[] args) {
        // 다운로드할 웹 페이지의 URL을 지정합니다.
        String webpageUrl = "https://www.example.com";
        
        try {
            // URL 객체를 생성합니다.
            URL url = new URL(webpageUrl);
            
            // HTTPURLConnection을 생성하고, URL과 연결합니다.
            HttpURLConnection connection = (HttpURLConnection) url.openConnection();
            connection.setRequestMethod("GET");
            
            // 응답 코드가 200 OK인 경우에만 페이지를 다운로드합니다.
            if (connection.getResponseCode() == HttpURLConnection.HTTP_OK) {
                // 입력 스트림을 받아와서 버퍼드 리더로 래핑합니다.
                BufferedReader reader = new BufferedReader(new InputStreamReader(connection.getInputStream()));
                
                // 한 줄씩 읽어와서 출력합니다.
                String line = null;
                while ((line = reader.readLine()) != null) {
                    System.out.println(line);
                }
                
                // 연결을 닫아줍니다.
                connection.disconnect();
            } else {
                System.out.println("페이지를 다운로드할 수 없습니다.");
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```
위 코드를 실행하면, 해당 웹 페이지의 HTML 코드를 출력합니다.

## 딥 다이브
웹 페이지를 다운로드하는 것은 웹 스크래핑, 크롤링 등 다양한 목적으로 사용될 수 있습니다. 또한, 다운로드한 페이지의 HTML을 파싱하여 원하는 정보를 추출하는 것도 가능합니다. 더 나아가서는, 다운로드한 페이지를 분석하여 특정 패턴이나 단어를 찾아내는 등의 작업도 가능합니다. Java 외에도 파이썬과 같은 다른 프로그래밍 언어에서도 유사한 기능을 구현할 수 있으니 참고하시기 바랍니다.

## 본문 끝
### 또 다른 참고 자료
- [Java HTTPURLConnection Docs](https://docs.oracle.com/javase/8/docs/api/java/net/HttpURLConnection.html)
- [웹 크롤링 튜토리얼](https://www.datacamp.com/community/tutorials/web-scraping-using-python)
- [오픈 소스 웹 크롤러 프레임워크, Jsoup](https://jsoup.org/)