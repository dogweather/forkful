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

## 왜

디렉토리가 존재하는지 확인하는 것이 왜 중요할까요? 여러분에게는 필요가 없지만 프로그램에게는 중요합니다. 만약 디렉토리가 존재하지 않으면 프로그램은 오류를 발생시키고 중지될 수 있습니다. 그래서 디렉토리가 존재하는지 미리 확인하는 것은 프로그램의 안정성을 높이는 중요한 일입니다.

## 방법

디렉토리가 존재하는지 확인하는 방법은 간단합니다. `File` 클래스의 `exists()` 메소드를 사용하면 됩니다. 다음은 `myDirectory`라는 이름의 디렉토리가 존재하는지 확인하는 예제 코드입니다.

```java
File myDirectory = new File("myDirectory");
if (myDirectory.exists()){
  System.out.println("myDirectory exists!");
} else {
  System.out.println("myDirectory does not exist.");
}
```

출력 결과는 아마 `myDirectory exists!`가 나올 것입니다. 만약 디렉토리가 존재하지 않는다면 `myDirectory does not exist.`가 출력될 것입니다. 

## 깊게 파보기

디렉토리를 확인할 때, 더 깊이 가보면 `isDirectory()` 메소드 또한 사용할 수 있습니다. 이 메소드는 디렉토리인지 아닌지 확인하여 boolean 값을 반환합니다. 만약 `myDirectory`이라는 파일 객체가 디렉토리라면, `myDirectory.isDirectory()`는 `true`를 반환할 것입니다. 

## 더 읽어보기

- [Java File Class Documentation](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [GeeksforGeeks Article on Checking if a File Exists in Java](https://www.geeksforgeeks.org/check-file-exists-java/)
- [Baeldung Article on Checking if a Directory Exists in Java](https://www.baeldung.com/java-check-directory-exists)