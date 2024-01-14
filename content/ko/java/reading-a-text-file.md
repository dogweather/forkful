---
title:                "Java: 텍스트 파일 읽기"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

"## 왜"

텍스트 파일을 읽는 것에 대해 많은 사람들이 궁금해 할 것입니다. 그 이유는 그것이 매우 유용하고 일상 생활에서 많이 사용된다는 것입니다.

"## 어떻게"

우선, 텍스트 파일을 읽기 위해서는 파일을 열어야 합니다. 그 후, 파일을 라인별로 읽고 내용을 출력하는 코드를 작성하면 됩니다.

```Java
File file = new File("textfile.txt"); 
BufferedReader br = new BufferedReader(new FileReader(file)); 
  
String line; 
while ((line = br.readLine()) != null) { 
    System.out.println(line); 
} 

```

위의 코드를 실행하면 텍스트 파일의 각 라인이 출력됩니다.

"## 깊이 파고들기"

텍스트 파일을 읽는 과정에서는 주의할 점이 있습니다. 첫째, 파일이 존재하는지 확인하는 것이 중요합니다. 둘째, 파일을 읽을 때 발생할 수 있는 예외를 처리해주어야 합니다. 마지막으로, 파일을 닫아줘야 합니다.

"See Also"

- [Java 파일 읽기 예제](https://www.w3schools.com/java/java_files_read.asp)
- [Java Exception 처리](https://www.codecademy.com/ko/learn/learn-java/modules/learn-java-exceptions/cheatsheet)
- [Java 파일 입출력](https://www.cs.princeton.edu/courses/archive/spr96/cs333/java/tutorial/java/io/fileinputstream.html)