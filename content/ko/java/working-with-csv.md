---
title:                "Java: csv 작업하기"
simple_title:         "csv 작업하기"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/working-with-csv.md"
---

{{< edit_this_page >}}

## 왜

CSV는 Comma Separated Values의 약자로서 데이터를 쉼표로 구분하여 저장하는 파일 형식입니다. 고려해볼 만한 이유는 데이터 분석이나 데이터베이스 관리자가 아니더라도 손쉽게 데이터를 읽고 쓸 수 있다는 것입니다. 또한 CSV 파일은 다양한 소프트웨어에서 지원되므로 편리하게 사용할 수 있습니다.

## 사용 방법

CSV 파일을 읽고 쓰기 위해서는 Java에서 제공하는 ```FileReader```와 ```FileWriter``` 클래스를 사용하면 됩니다. 먼저 파일을 읽어올 때는 다음과 같은 코드를 사용합니다.

```Java
FileReader reader = new FileReader("data.csv");
BufferedReader bufferedReader = new BufferedReader(reader);
String line = bufferedReader.readLine();
while (line != null) {
  // 데이터 처리 코드
  line = bufferedReader.readLine();
}
bufferedReader.close();
```

위 코드에서 ```readLine()``` 메소드를 이용하여 한 줄씩 데이터를 읽어올 수 있습니다. 데이터 처리를 마치고 나서는 ```FileWriter```를 이용하여 파일에 쓰는 것도 비슷한 방식으로 해줄 수 있습니다.

```Java
FileWriter writer = new FileWriter("output.csv");
BufferedWriter bufferedWriter = new BufferedWriter(writer);
bufferedWriter.write("output data");
bufferedWriter.newLine();
bufferedWriter.close();
```

추가적으로 데이터를 구분하는 쉼표나 다른 delimiter를 변경하고 싶다면 ```CSVParser``` 클래스를 사용하여 설정해줄 수 있습니다.

## 딥 다이브

CSV 파일을 다루는 더 깊은 내용으로는 데이터 타입 변환, 불러오는 데이터의 오류 처리, CSV 파일의 헤더 정보 등이 있습니다. 데이터 타입 변환은 데이터를 읽어올 때 String으로 읽어오고, 필요한 경우 변환 작업을 해주어야 합니다. 예를 들어 숫자 데이터를 읽어올 때는 ```Integer.parseInt()``` 메소드를 사용하여 String을 정수로 변환할 수 있습니다.

반면, 데이터를 CSV 파일에 쓸 때도 데이터 타입에 맞게 변환하여 써주어야 합니다. 또한 오류 처리를 위해 ```try-catch``` 구문을 사용하여 예외 상황에 대비하는 것이 좋습니다. 또한 CSV 파일이 헤더 정보를 포함하고 있는 경우가 많은데, 이 정보를 이용하여 데이터를 읽어오거나 정렬하는 등의 작업을 할 수 있습니다.

## 알아보기

더 많은 CSV 파일 다루는 방법과 관련한 자료는 아래 링크들을 참고해주세요.

- [Java CSV 포맷으로 파일 쓰기](https://www.baeldung.com/java-csv-file-array)
- [Java CSV 파일 다루기 예제](https://www.journaldev.com/12501/java-csv-file-reader-and-writer-example-csv-aggregator#java-read-csv-file)
- [Apache CSV 라이브러리 예제](https://www.byteslounge.com/tutorials/apache-commons-csv-example)
- [CSV 파일 작성 자바 코드 예제](https://mkyong.com/java/how-to-read-and-parse-csv-file-in-java/)

참고 자료에서는 CSV 파일을 읽고 쓰는 방법뿐만 아니라 CSV 파일 형식에 대한 자세한 설명도 찾아볼 수 있습니다.