---
title:                "Java: 웹 페이지 다운로드"
simple_title:         "웹 페이지 다운로드"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 왜

웹 페이지를 다운로드하는 이유는 인터넷에서 정보를 수집하거나 웹 스크레이핑을 할 때에 필요합니다.

## 방법

웹 페이지를 다운로드하기 위해서는 Java를 사용하여 URL을 연결하고, 연결된 스트림에서 데이터를 읽어오는 방식을 사용합니다. 아래는 해당 기능을 구현한 예제 코드입니다.

```Java
import java.net.*;
import java.io.*;

public class WebPageDownloader {
  public static void main(String[] args) {
    try {
      // 다운로드할 웹 페이지의 URL
      URL url = new URL("https://example.com/");

      // URL 연결
      URLConnection con = url.openConnection();

      // 연결된 스트림을 읽기 위한 InputStream 생성
      InputStream in = con.getInputStream();

      // 읽어온 데이터를 저장하기 위한 StringBuilder 생성
      StringBuilder sb = new StringBuilder();

      // 스트림에서 데이터를 읽어와서 StringBuilder에 저장
      InputStreamReader isr = new InputStreamReader(in);
      BufferedReader br = new BufferedReader(isr);
      String line;
      while ((line = br.readLine()) != null) {
        sb.append(line);
      }

      // StringBuilder에 저장된 데이터 출력
      System.out.println(sb.toString());
    } catch (Exception e) {
      e.printStackTrace();
    }
  }
}
```

위 코드를 실행하면 해당 웹 페이지의 HTML 코드가 출력됩니다.

## 딥 다이브

실제로 웹 페이지를 다운로드하는 과정은 위 예제 코드에서 말씀드린 것보다 더 복잡합니다. 내부적으로 여러 가지 프로토콜과 인코딩 방식이 사용되지만, 우리는 Java를 사용하여 이 모든 것을 쉽게 처리할 수 있습니다.

## 추가 정보

[Java Official Documentation on URL Connection](https://docs.oracle.com/javase/8/docs/api/java/net/URLConnection.html).

[Java Official Tutorial on Networking](https://docs.oracle.com/javase/tutorial/networking/).

## 확인하기

Java를 사용하여 웹 페이지를 다운로드하는 방법은 매우 유용하며 다양한 분야에서 활용될 수 있습니다. 위 코드를 실행해보고, 다른 웹 페이지를 다운로드하는 기능을 추가해보는 등 다양한 실험을 해보세요.