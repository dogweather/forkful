---
title:                "디렉토리가 존재하는지 확인하기"
html_title:           "Java: 디렉토리가 존재하는지 확인하기"
simple_title:         "디렉토리가 존재하는지 확인하기"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

디렉터리가 존재하는지 확인하는 것은 프로그래머들이 프로그램을 작성할 때 중요한 작업입니다. 프로그램이 파일이나 폴더를 읽거나 쓸 때, 디렉터리가 존재하는지 여부를 미리 확인하는 것으로, 프로그램의 안정성을 높일 수 있습니다. 

## 방법:

```Java 
File file = new File("C:/Users/username/Desktop/myDirectory"); 

if(file.exists() && file.isDirectory()) { 
  System.out.println("myDirectory exists."); 
} else { 
  System.out.println("myDirectory does not exist."); 
}
```

위의 예제에서는 "C:/Users/username/Desktop/myDirectory" 경로의 디렉터리가 존재하는지를 확인합니다. 만약 디렉터리가 존재하지 않으면, "myDirectory does not exist." 메시지가 출력됩니다. 

## 깊이 들어가보기:

(1) 디렉터리가 존재하는지 확인하는 것은 오래 전부터 프로그래밍에서 자주 사용되는 기술입니다. (2) 다른 방법으로는 예외처리를 사용하여 디렉터리를 확인하는 것이 있지만, 이는 코드를 더 복잡하게 만듭니다. (3) Java에서는 File 클래스의 exists()와 isDirectory() 메소드를 사용하여 디렉터리가 존재하는지를 확인할 수 있습니다. 

## 관련 자료:

[Java File Class Documentation](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)