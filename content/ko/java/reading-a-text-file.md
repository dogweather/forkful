---
title:    "Java: 텍스트 파일 읽기"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 왜?

텍스트 파일을 읽는 것은 프로그래밍에서 매우 중요한 부분입니다. 텍스트 파일은 데이터를 저장하고 전송하는 데에 매우 유용하며, 이를 프로그래밍에서 사용하기 위해서는 파일을 읽어야 합니다. 이 글을 읽는 것으로 여러분은 자바에서 텍스트 파일을 읽는 방법을 배울 수 있습니다.

## 어떻게?

```Java
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class ReadTextFile {

    public static void main(String[] args) {
        File file = new File("sample.txt");
        // 파일을 읽어옵니다.
        try {
            // Scanner 클래스를 사용하여 파일 내용을 순서대로 읽습니다.
            Scanner fileReader = new Scanner(file);
            // 파일의 끝까지 반복합니다.
            while (fileReader.hasNextLine()) {
                // 현재 줄을 읽어옵니다.
                String data = fileReader.nextLine();
                // 파일의 내용을 출력합니다.
                System.out.println(data);
            }
            // 파일을 닫아줍니다.
            fileReader.close();
        } catch (FileNotFoundException e) {
            System.out.println("파일을 찾을 수 없습니다.");
            e.printStackTrace();
        }
    }
}
```

위의 코드는 "sample.txt" 파일을 읽고 콘솔에 내용을 출력하는 간단한 예시입니다. Java의 File 클래스를 사용하여 파일을 읽어온 후 Scanner 클래스를 사용하여 파일 내용을 순서대로 읽어옵니다. 반복문을 사용하여 파일의 끝까지 반복하며, 한 줄씩 읽어온 후 콘솔에 출력합니다. 마지막으로 파일을 닫아줍니다.

```Java
Output:

Hello World!
안녕하세요
こんにちは
```

위의 출력결과는 "sample.txt" 파일의 내용을 출력한 것입니다. 파일의 내용이 순서대로 출력되는 것을 확인할 수 있습니다.

## 깊이있게 살펴보기

자바에서 텍스트 파일을 읽을 때 가장 많이 사용되는 클래스는 File 클래스와 Scanner 클래스입니다. File 클래스는 파일을 읽고 쓰기 위한 기본적인 메소드를 제공하며, Scanner 클래스는 파일을 읽어오는 기능을 제공합니다.

또한 파일을 읽을 때 주의해야 할 점은 파일의 경로를 정확하게 지정해주어야 한다는 것입니다. 파일의 경로를 잘못 지정하면 파일을 찾을 수 없는 에러가 발생할 수 있습니다. 또한 파일을 모두 읽은 후에는 반드시 파일을 닫아주어야 합니다.

## See Also
"See Also" (참고)  


- Oracle Java Tutorials (오피셜 자바 튜토리얼): https://docs.oracle.com/javase/tutorial/essential/io/file.html
- W3Schools Java File I/O (W3Schools 자바 파일 입출력): https://www.w3schools.com/java/java_files.asp