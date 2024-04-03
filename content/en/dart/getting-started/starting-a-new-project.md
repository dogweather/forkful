---
date: 2024-03-08 21:33:26.692396-07:00
description: "Starting a new project in Dart involves setting up an environment conducive\
  \ to efficient development, testing, and deployment. Programmers initiate new\u2026"
lastmod: '2024-03-13T22:44:59.821410-06:00'
model: gpt-4-0125-preview
summary: Starting a new project in Dart involves setting up an environment conducive
  to efficient development, testing, and deployment.
title: Starting a new project
weight: 1
---

## What & Why?

Starting a new project in Dart involves setting up an environment conducive to efficient development, testing, and deployment. Programmers initiate new Dart projects to leverage Dart's optimal performance and robust ecosystem, particularly for web and mobile app development with frameworks like Flutter.

## How to:

1. **Install Dart**:
   Ensure Dart is installed on your system. If not, you can download it from [https://dart.dev/get-dart](https://dart.dev/get-dart). Verify the installation with:

   ```shell
   dart --version
   ```

2. **Create a New Dart Project**:
   Use the Dart CLI to generate a new project:

   ```shell
   dart create hello_dart
   ```

   This command creates a new directory `hello_dart` with a simple sample web or console application, depending on your selection.

3. **Examine the Project Structure**:
   
   Navigate to your project directory:

   ```shell
   cd hello_dart
   ```

   A typical Dart project includes the following key files and directories:

   - `pubspec.yaml`: Configuration file that includes your project's dependencies and SDK constraints.
   - `lib/`: Directory where most of the Dart code resides.
   - `test/`: Directory for project tests.

4. **Add Dependencies**:
   Edit `pubspec.yaml` to add dependencies. For web projects, consider adding `http`, a popular package for making HTTP requests:

   ```yaml
   dependencies:
     flutter:
       sdk: flutter
     http: ^0.13.3
   ```

   After editing, get the dependencies:

   ```shell
   dart pub get
   ```

5. **Write Your First Dart Code**:
   
   In the `lib/` directory, create a new Dart file, `main.dart`, and add a simple Dart code:

   ```dart
   // Import the Dart core library
   import 'dart:core';

   void main() {
     print('Hello, Dart!');
   }
   ```

6. **Run Your Dart Application**:

   Execute your Dart program with:

   ```shell
   dart run
   ```

   The output should be:

   ```
   Hello, Dart!
   ```

By following these steps, you've successfully started a new Dart project, from installation to running your first piece of Dart code. This foundational knowledge sets the stage for diving deeper into Dart's rich ecosystem and its capabilities for building scalable applications.
