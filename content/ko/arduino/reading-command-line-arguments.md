---
title:                "컴퓨터 프로그래밍을 위한 명령줄 인수 읽기"
html_title:           "Arduino: 컴퓨터 프로그래밍을 위한 명령줄 인수 읽기"
simple_title:         "컴퓨터 프로그래밍을 위한 명령줄 인수 읽기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# 무엇 & 왜? 
Arduino 프로그래밍에는 명령 줄 인수를 읽는 데 필요한 기능이 있습니다. 이는 사용자의 입력을 읽어 프로그램을 실행하는 데 도움이 됩니다. 프로그래머는 이 기능을 사용하여 새로운 기능을 추가하거나 사용자의 입력에 따라 다른 작업을 수행할 수 있습니다.

# 어떻게: 
Arduino에는 두 가지 방법으로 명령 줄 인수를 읽을 수 있습니다. 첫 번째 방법은 간단한 코드를 사용하는 것이고, 두 번째 방법은 조금 더 복잡하지만 다양한 인수를 처리할 수 있습니다.

### 간단한 코드 예제:
```Arduino
int main(int argc, char *argv[]) {
  Serial.begin(9600);
  Serial.print("입력 받은 인수의 개수: ");
  Serial.println(argc); // 인수의 개수 출력
  Serial.println("인수 목록:");
  for (int i=0; i<argc; i++) { // 인수 목록 출력
    Serial.println(argv[i]); 
  }
}
```

### 입력 예제:
```Arduino
python run.py "Hello" 1 2 3
```

### 출력:
```
입력 받은 인수의 개수: 5
인수 목록:
run.py
Hello
1
2
3
```

### 복잡한 코드 예제:
```Arduino
int main(int argc, char *argv[]) {
  Serial.begin(9600);
  int num = 0;
  for (int i=1; i<argc; i++) { // 첫 번째 인수를 제외한 나머지 인수를 숫자로 변환하여 더하기
    num += String(argv[i]).toInt(); 
  }
  Serial.print("입력 받은 숫자들의 합: ");
  Serial.println(num); // 숫자의 합 출력
}
```

### 입력 예제:
```Arduino
python run.py 1 2 "3" 4
```

### 출력:
```
입력 받은 숫자들의 합: 10
```

# 깊게 들어가기:
명령 줄 인수를 읽는 기능은 컴퓨터 프로그래밍의 많은 부분에서 사용됩니다. 이 기능은 유닉스 시스템에서 유래되었으며, C언어에서 많이 사용됩니다. Arduino에서는 문자열 배열을 사용하여 입력된 인수들을 저장하고 사용할 수 있습니다. 또한, 입력된 인수가 올바른 형식의 데이터인지 검증하는 등의 추가적인 작업도 가능합니다.

# 더 알아보기:
- [Arduino Command-line arguments](https://www.arduino.cc/en/Tutorial/CommandLineArguments): Arduino 공식 홈페이지의 명령 줄 인수 관련 튜토리얼입니다.
- [Introduction to C Command-line Arguments](https://www.geeksforgeeks.org/introduction-to-c-programming/?ref=lbp): C언어에서 명령 줄 인수를 처리하는 방법에 대해 자세히 설명하고 있습니다.