---
title:    "Java: 텍스트 파일 읽기"
keywords: ["Java"]
---

{{< edit_this_page >}}

# 왜?

한국어 블로그 관련 정보를 찾는 분들은 분명 Java 프로그래밍에 관심이 있을 것입니다. 텍스트 파일을 읽는 것은 Java 프로그래밍에서 매우 중요한 부분입니다. 그래서 지금부터 텍스트 파일을 읽는 방법에 대해 설명드리겠습니다.

## 어떻게?

```Java
import java.io.File;
import java.util.Scanner;

public class ReadFile {

    public static void main(String[] args) {
        
        // 파일 경로 설정
        File file = new File("example.txt");
        
        try {
            // Scanner를 사용하여 파일 읽기
            Scanner scanner = new Scanner(file);
            
            // 한 줄씩 읽으며 출력
            while (scanner.hasNextLine()) {
                System.out.println(scanner.nextLine());
            }
            
            // Scanner 닫기
            scanner.close();
            
        } catch (Exception e) {
            System.out.println("파일을 읽을 수 없습니다.");
        }
    }
}
```

위의 코드는 텍스트 파일을 읽고 출력하는 간단한 예제입니다. 파일을 올바른 경로에 두고 위의 코드를 실행하면 파일의 내용이 한 줄씩 출력될 것입니다. 파일을 읽을 때 발생할 수 있는 예외를 처리하는 것도 잊지 마세요.

```
Hello world!
안녕하세요
```

## 딥 다이브

한 줄씩 읽는 것 외에도 Scanner를 사용하여 파일 내용을 원하는 방식에 따라 읽을 수 있습니다. 예를 들어, ```next()``` 함수를 사용하면 공백이나 개행 문자까지 한 단어씩 읽을 수 있고, ```hasNextInt()``` 함수를 사용하면 숫자만을 읽을 수 있습니다. 자세한 내용은 Java 문서를 참고해 주세요.

## 참고

- [Java Scanner class](https://docs.oracle.com/javase/8/docs/api/java/util/Scanner.html)
- [Java File class](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [Java Reading and Writing Files](https://www.baeldung.com/java-read-write-file)