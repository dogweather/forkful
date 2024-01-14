---
title:                "Arduino: 디렉토리가 존재하는지 확인하기"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜

디렉토리가 존재하는지 확인하는 것의 이유는 매우 간단합니다. 우리는 특정 디렉토리 내에 파일이 존재하는지 여부를 알아야 할 때가 있습니다. 예를 들어, 사용자가 작성한 파일을 읽거나 새 파일을 생성하려는 경우 해당 디렉토리가 존재하는지 확인하는 것이 필요합니다.

## 방법

디렉토리의 존재 여부를 확인하는 방법은 아두이노에서 매우 간단합니다. 우리는 `exists()` 함수를 사용하여 해당 디렉토리의 존재 여부를 확인할 수 있습니다. 이 함수는 boolean 값을 반환하며 디렉토리가 존재하면 `true`를, 존재하지 않으면 `false`를 반환합니다.

```Arduino
if (exists("/data")){
  Serial.println("디렉토리가 존재합니다!");
}
else {
  Serial.println("디렉토리가 존재하지 않습니다.");
}
```

위의 코드 예제에서는 "/data" 디렉토리가 존재하는지 여부를 확인하고 그에 따라 메시지를 출력합니다.

## 깊게 파헤치기

보다 깊이있게 디렉토리의 존재 여부를 이해하기 위해서는 파일 시스템이 어떻게 작동하는지를 이해하는 것이 중요합니다. 아두이노에서는 SD 라이브러리를 사용하여 파일 시스템을 다룰 수 있습니다. 파일 시스템은 파일이나 디렉토리를 나열하는 일련의 데이터 블록으로 구성되어 있습니다. 그리고 디렉토리는 파일 시스템에서 특정 데이터 블록을 가리키는 역할을 합니다. 즉, 해당 디렉토리가 존재한다는 것은 파일 시스템에서 그 데이터 블록이 존재한다는 것을 의미합니다.

## 또 다른 관련 항목

- [SD 라이브러리 문서](https://www.arduino.cc/en/Reference/SD)
- [거북이쉘 블로그 "보기"](https://www.turtleshell.co.kr/view.php?id=adarduinoforbeginners&page=1&sn1=usage&divpage=1&sn=off&ss=on&sc=on&select_arrange=headnum&desc=asc&no=5)
- [아두이노 공식 포럼](https://forum.arduino.cc/)