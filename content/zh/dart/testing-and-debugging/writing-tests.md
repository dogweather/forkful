---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:59.664849-07:00
description: "\u5728 Dart \u4E2D\u7F16\u5199\u6D4B\u8BD5\uFF0C\u6D89\u53CA\u521B\u5EFA\
  \u6D4B\u8BD5\u7528\u4F8B\u4EE5\u81EA\u52A8\u9A8C\u8BC1\u7A0B\u5E8F\u7684\u4E0D\u540C\
  \u90E8\u5206\u662F\u5426\u6309\u9884\u671F\u5DE5\u4F5C\u3002\u7A0B\u5E8F\u5458\u8FD9\
  \u6837\u505A\u662F\u4E3A\u4E86\u786E\u4FDD\u4ED6\u4EEC\u7684\u4EE3\u7801\u53EF\u9760\
  \u4E14\u65E0\u7F3A\u9677\uFF0C\u4ECE\u800C\u4FBF\u4E8E\u66F4\u5BB9\u6613\u5730\u66F4\
  \u65B0\u548C\u91CD\u6784\uFF0C\u540C\u65F6\u9632\u6B62\u56DE\u5F52\u3002"
lastmod: '2024-03-11T00:14:21.181276-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Dart \u4E2D\u7F16\u5199\u6D4B\u8BD5\uFF0C\u6D89\u53CA\u521B\u5EFA\
  \u6D4B\u8BD5\u7528\u4F8B\u4EE5\u81EA\u52A8\u9A8C\u8BC1\u7A0B\u5E8F\u7684\u4E0D\u540C\
  \u90E8\u5206\u662F\u5426\u6309\u9884\u671F\u5DE5\u4F5C\u3002\u7A0B\u5E8F\u5458\u8FD9\
  \u6837\u505A\u662F\u4E3A\u4E86\u786E\u4FDD\u4ED6\u4EEC\u7684\u4EE3\u7801\u53EF\u9760\
  \u4E14\u65E0\u7F3A\u9677\uFF0C\u4ECE\u800C\u4FBF\u4E8E\u66F4\u5BB9\u6613\u5730\u66F4\
  \u65B0\u548C\u91CD\u6784\uFF0C\u540C\u65F6\u9632\u6B62\u56DE\u5F52\u3002"
title: "\u7F16\u5199\u6D4B\u8BD5"
---

{{< edit_this_page >}}

## 什么 & 为什么？

在 Dart 中编写测试，涉及创建测试用例以自动验证程序的不同部分是否按预期工作。程序员这样做是为了确保他们的代码可靠且无缺陷，从而便于更容易地更新和重构，同时防止回归。

## 如何操作：

在 Dart 中，通常使用 `test` 包来编写测试。首先，将 `test` 包添加到你的 `pubspec.yaml` 中：

```yaml
dev_dependencies:
  test: ^1.0.0
```

然后，为一个简单的函数编写测试。假设你有一个添加两个数字的函数：

```dart
int add(int a, int b) {
  return a + b;
}
```

接着，在 `test` 目录中创建一个名为 `add_test.dart` 的文件，并编写你的测试用例：

```dart
import 'package:test/test.dart';
import '../lib/add.dart'; // 假设你的 `add` 函数在 lib/add.dart 中

void main() {
  test('两数相加', () {
    var expected = 3;
    expect(add(1, 2), equals(expected));
  });
}
```

要运行测试，使用 Dart 命令：

```bash
$ dart test
```

样本输出可能类似于：

```
00:01 +1: 所有测试通过！
```

### 使用第三方库：Mockito 进行模拟

对于具有复杂依赖关系的代码测试，你可能会使用 Mockito 来创建模拟对象。首先，将 Mockito 添加到你的 `pubspec.yaml`：

```yaml
dev_dependencies:
  mockito: ^5.0.0
```

假设你有一个 `UserRepository` 类，用于获取用户数据，并且你想测试一个依赖于 `UserRepository` 的 `UserService`，而不触及真实数据库：

```dart
import 'package:mockito/mockito.dart';
import 'package:test/test.dart';
import 'package:your_project/user_repository.dart';
import 'package:your_project/user_service.dart';

// 使用 Mockito 创建一个 Mock 类
class MockUserRepository extends Mock implements UserRepository {}

void main() {
  group('UserService 测试', () {
    test('成功获取用户', () {
      // 创建模拟实例
      final mockUserRepository = MockUserRepository();
      final userService = UserService(mockUserRepository);

      // 设置模拟行为
      when(mockUserRepository.fetchUser(1)).thenReturn(User(id: 1, name: '测试用户'));

      // 断言模拟方法使用预期参数被调用
      expect(userService.getUserName(1), '测试用户');
      verify(mockUserRepository.fetchUser(1)).called(1);
    });
  });
}
```

运行此测试确认 `UserService` 正确地与 `UserRepository` 交互，使用模拟来以受控方式模拟真实交互。
