---
title:                "Java: 텍스트 파일 쓰기"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

텍스트 파일 작성에 참여하는 이유는 무엇일까요? 텍스트 파일은 컴퓨터에서 데이터를 저장하고 전송하기 위한 가장 기본적인 형식입니다. 일반적으로 우리는 여러 가지 데이터를 보관하고 관리하는 데 사용하는 텍스트 파일을 자주 사용합니다.

## 작성 방법

자바에서 텍스트 파일을 작성하는 방법은 간단합니다. 먼저, FileWriter 클래스를 사용하여 파일을 생성하고 FileWriter 객체를 이용하여 파일을 작성하면 됩니다. 아래의 예시 코드를 참고해보세요.

```Java
import java.io.FileWriter;  

public class TextFileWriter {  
    public static void main(String[] args) {  
        try {  
            FileWriter fw = new FileWriter("myTextFile.txt");  
            fw.write("안녕하세요, 이것은 텍스트 파일 쓰기 예제입니다.");  
            fw.close();  
            System.out.println("텍스트 파일이 성공적으로 작성되었습니다.");  
        } 
        catch(Exception e) {  
            System.out.println(e);  
        }  
    }  
}
```

위의 코드를 실행하면 "myTextFile.txt"라는 이름의 텍스트 파일이 생성됩니다. 파일을 열어보면 "안녕하세요, 이것은 텍스트 파일 쓰기 예제입니다."라는 문구가 작성되어 있는 것을 볼 수 있습니다.

## 깊은 곳으로

텍스트 파일을 작성하는 것은 매우 간단하고 기본적인 작업입니다. 하지만 더 나아가서 파일을 생성한 후에는 다양한 작업을 수행할 수 있습니다. 예를 들어, FileWriter 클래스에서 제공하는 다양한 메소드를 이용하여 파일에 내용을 추가하거나, 파일의 내용을 읽어오는 것도 가능합니다. 또한, 파일을 생성할 때 인코딩 방식을 지정하여 다양한 언어의 텍스트를 저장하고 읽어올 수도 있습니다.

## 또 다른 정보들

- [Java.io API 문서](https://docs.oracle.com/javase/8/docs/api/java/io/package-summary.html)
- [자바로 파일 입출력하기](https://codechacha.com/ko/java-read-write-files/)
- [Java 텍스트 파일 작성하기](https://zetcode.com/java/filewriter/)
- [자바 입출력 스트림(InputStream, OutputStream, FileReader, FileWriter)](https://codechacha.com/ko/java-io-streams/)