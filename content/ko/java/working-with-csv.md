---
title:                "CSV 파일 작업"
html_title:           "Java: CSV 파일 작업"
simple_title:         "CSV 파일 작업"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/working-with-csv.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

CSV란 무엇일까요? CSV는 Comma Separated Values의 약자로, 콤마(,)로 구분된 데이터의 집합입니다. 프로그래머들은 CSV를 사용하는 이유는 데이터를 효율적으로 저장하고 검색하기 위해서입니다.

## 방법:

자바에서 CSV를 다루는 방법을 살펴보겠습니다. 먼저 CSV 파일을 읽고 쓰기 위해서는 File, FileReader, FileWriter 등의 클래스를 사용해야 합니다. 아래 예시 코드를 참고해보세요.

```Java
import java.io.*;
public class CSVExample {
  public static void main(String[] args) {
    try {
        File file = new File("data.csv");
        // CSV 파일 읽기
        String line = "";
        BufferedReader br = new BufferedReader(new FileReader(file));
        while ((line = br.readLine()) != null) {
          // 쉼표를 기준으로 분리
          String[] data = line.split(",");
          // 데이터 출력
          System.out.println("Name: " + data[0] + ", Age: " + data[1] + ", City: " + data[2]);
        }
        br.close();
        // CSV 파일 쓰기
        FileWriter writer = new FileWriter("data.csv");
        // 데이터 추가
        writer.append("John,25,New York\n");
        writer.append("Maria,30,Seoul\n");
        writer.close();
    } catch (Exception e) {
      e.printStackTrace();
    }
  }
}
```

**출력:**
```
Name: John, Age: 25, City: New York
Name: Maria, Age: 30, City: Seoul
```

## 깊이 파고들기:

CSV의 역사적 관점, 대안, 그리고 구현 세부사항에 대해 알아보겠습니다. CSV는 1972년 개발된 데이텀 방식의 데이터 포맷입니다. 텍스트 파일로 인식되기 때문에 다양한 프로그래밍 언어에서 이용할 수 있습니다. CSV 대안으로는 JSON, XML 등이 있으며, 다양한 라이브러리를 통해 좀 더 편리하게 CSV를 다룰 수 있습니다. 자바에서도 OpenCSV, Apache Commons CSV 등의 라이브러리를 사용할 수 있습니다.

## 관련 자료:

- Wikipedia: [CSV 파일](https://ko.wikipedia.org/wiki/CSV_%ED%8C%8C%EC%9D%BC)
- Oracle Java Tutorials: [Reading, Writing, and Creating Files](https://docs.oracle.com/javase/tutorial/essential/io/file.html)
- OpenCSV: [Home Page](http://opencsv.sourceforge.net/)
- Apache Commons CSV: [User Guide](https://commons.apache.org/proper/commons-csv/user-guide.html)