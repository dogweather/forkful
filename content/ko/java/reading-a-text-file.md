---
title:                "텍스트 파일 읽기"
html_title:           "Java: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

# 왜

이 문서를 읽는 이유는 텍스트 파일을 읽는 방법을 배우고 싶어서 일 것입니다. Java 프로그래밍 언어를 이용해 텍스트 파일을 읽는 방법을 설명해 드리겠습니다.

## 어떻게

텍스트 파일을 읽기 위해서는 ````Java``` 코드 블록 안에 다음 코드를 작성해야 합니다.

````Java
import java.io.*;

public class ReadFile {
    public static void main(String[] args){
        // 파일 경로 설정
        File file = new File("Example.txt");
        
        try{
            // 파일 읽기
            BufferedReader br = new BufferedReader(new FileReader(file));
            String line;
            
            // 한 줄씩 읽어서 출력
            while((line = br.readLine()) != null){
                System.out.println(line);
            }
            
            // 파일 닫기
            br.close();
        } catch (IOException e) {
            System.out.println("파일을 읽는 중 오류 발생: " + e.getMessage());
        }
    }
}
````

위 코드는 ``Example.txt`` 파일을 한 줄씩 읽어서 콘솔에 출력하는 예제입니다. 만약 파일을 읽는 도중 오류가 발생하면 해당 오류 메시지가 출력됩니다.

## 딥 다이브

텍스트 파일을 읽기 위해서는 올바른 파일 경로를 설정해야 합니다. 또한 파일을 열고 사용한 뒤에는 반드시 파일을 닫아야 합니다. 그렇지 않으면 파일이 사용 중인 상태로 남아 있게 되어 다른 프로그램에서 파일을 사용할 수 없게 됩니다. 더 많은 정보를 원한다면 Java 공식 문서를 참고해보세요.

# 참고

[Java 공식 문서](https://docs.oracle.com/javase/8/docs/api/java/io/BufferedReader.html)

# 관련 글

[Java로 텍스트 파일 쓰기](https://github.com/ShinAhYoung21/Java-File-Writing-Article/blob/main/article_ko.md)