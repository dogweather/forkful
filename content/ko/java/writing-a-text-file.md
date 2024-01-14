---
title:    "Java: 텍스트 파일 작성하기"
keywords: ["Java"]
---

{{< edit_this_page >}}

## 왜

텍스트 파일을 생성하는 이유는 우리가 작성한 데이터를 영구적으로 저장하기 위해서입니다. 이는 프로그래밍에서 중요한 정보를 보관하는 데 매우 유용합니다.

## 방법

텍스트 파일을 생성하는 방법은 다양합니다. 여기서는 주요한 두 가지 방법을 소개하겠습니다.

### FileWriter 클래스 이용하기

```java
import java.io.FileWriter;

public class TextFileWriterExample {

    public static void main(String[] args) {

        try {
            FileWriter writer = new FileWriter("output.txt"); //파일이 저장될 경로와 파일 이름
            writer.write("안녕하세요, Java 프로그래밍을 배우고 있습니다."); //파일에 작성할 내용
            writer.close(); //파일 작성을 완료하고 닫기
            
            System.out.println("파일이 성공적으로 생성되었습니다.");
        } catch (Exception e) {
            System.out.println(e.getMessage());
        }
    }
}
```

위의 코드를 실행하면 같은 디렉토리에 "output.txt" 파일이 생성됩니다. 이 파일을 열어보면 "안녕하세요, Java 프로그래밍을 배우고 있습니다." 라는 내용이 적혀있는 것을 확인할 수 있습니다.

### PrintWriter 클래스 이용하기

```java
import java.io.PrintWriter;

public class TextFileWriterExample {

    public static void main(String[] args) {

        try {
            PrintWriter pw = new PrintWriter("output.txt"); //파일이 저장될 경로와 파일 이름
            pw.println("화이팅, Java 프로그래밍!"); //파일에 작성할 내용, println 메소드를 이용해 줄 바꿈 가능
            pw.close(); //파일 작성을 완료하고 닫기

            System.out.println("파일이 성공적으로 생성되었습니다.");
        } catch (Exception e) {
            System.out.println(e.getMessage());
        }
    }
}
```

위의 코드를 실행하면 같은 디렉토리에 "output.txt" 파일이 생성됩니다. 이 파일을 열어보면 "화이팅, Java 프로그래밍!" 이라는 내용이 적혀있는 것을 확인할 수 있습니다.

## 깊은 곳으로

텍스트 파일을 생성하는 데에는 여러 가지 옵션과 기능들이 있습니다. 우리는 파일을 읽어올 때와 마찬가지로 FileWriter 클래스 또는 PrintWriter 클래스를 사용하여 내용을 덮어쓰거나 이어쓸 수 있습니다. 또한 BufferedWriter 클래스와 같이 문자열을 버퍼에 저장한 후 한번에 파일에 작성할 수도 있습니다. 더욱 깊게 이해하고 싶다면 관련 자료와 책을 참고해보시기 바랍니다.

## 같이 보기

- [Java FileWriter 클래스](https://docs.oracle.com/javase/8/docs/api/java/io/FileWriter.html)
- [Java PrintWriter 클래스](https://docs.oracle.com/javase/8/docs/api/java/io/PrintWriter.html)
- [Java BufferedWriter 클래스](https://docs.oracle.com/javase/8/docs/api/java/io/BufferedWriter.html)