---
title:                "csv 파일 작업하기"
html_title:           "Java: csv 파일 작업하기"
simple_title:         "csv 파일 작업하기"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/working-with-csv.md"
---

{{< edit_this_page >}}

## 왜

CSV 파일은 데이터를 저장하고 공유하기에 매우 간편한 형식입니다. 따라서 Java 프로그래밍을 하고 있는 개발자들은 이 파일 형식을 자주 만나게 됩니다. CSV 파일을 다루는 법을 배우면 데이터를 손쉽게 다룰 수 있고, 이는 프로그래밍 작업을 더욱 효율적으로 만들어 줍니다.

## 어떻게

CSV 파일을 다루기 위해서는 FileReader와 BufferedReader 클래스를 사용하여 파일을 읽어와야 합니다. 이후에는 split() 메소드를 사용하여 데이터를 행 단위로 분리하고 각 열의 값을 추출할 수 있습니다. 아래의 예시를 참고하세요.

```Java
import java.io.BufferedReader;
import java.io.FileReader;

public class CSVReader {

  public static void main(String[] args) {

    try {
    
      // CSV 파일을 읽기 위해 FileReader를 사용합니다.
      FileReader fr = new FileReader("data.csv");
      
      // FileReader의 결과를 BufferedReader로 읽습니다.
      BufferedReader br = new BufferedReader(fr);
      
      // 데이터를 읽어올 변수를 선언합니다.
      String row;
      
      // while 루프를 사용하여 CSV 파일의 모든 데이터를 읽어옵니다.
      while ((row = br.readline()) != null) {
      
        // 각 행을 콤마(,)로 분리하여 데이터를 배열로 저장합니다.
        String[] data = row.split(",");
        
        // 각 열의 값을 출력합니다.
        System.out.println("Name: " + data[0]);
        System.out.println("Age: " + data[1]);
        System.out.println("Grade: " + data[2]);
        
      }
      
      // 파일을 닫습니다.
      br.close();
    
    } catch (Exception e) {
      e.printStackTrace();
    }
  }
}
```

이 예시를 실행하면, 아래와 같은 출력을 얻게 됩니다.

```
Name: John
Age: 25
Grade: A
Name: Emily
Age: 22
Grade: B
Name: David
Age: 27
Grade: A+
```

## 딥 다이브

CSV 파일을 다루는 데에는 여러 방법이 있습니다. 예를 들어, CSV 파일에 새로운 데이터를 추가하는 방법, 특정 조건에 맞는 데이터를 검색하는 방법 등이 있습니다. 더 많은 기능을 알아보고 싶다면, Java에서 지원하는 CSV 라이브러리를 참조해보시기 바랍니다.

## 관련 링크

- [Java CSV 라이브러리](https://zhishixiang.com/p/how-to-read-and-write-csv-file-in-java)
- [CSV 파일 다루기 예제](https://www.codejava.net/java-se/file-io/parse-csv-files-in-java)
- [Java에서 CSV 파일 쓰기](https://stackoverflow.com/questions/27331479/java-new-lines-not-being-generated-in-csv-file)
- [Java FileReader와 BufferedReader에 대한 자세한 설명](https://docs.oracle.com/javase/7/docs/api/java/io/BufferedReader.html)