---
title:                "Starting a new project"
html_title:           "Bash recipe: Starting a new project"
simple_title:         "Starting a new project"
programming_language: "Java"
category:             "Java"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why?

Starting a new project in Java means initializing a fresh workspace to build a new application or system from scratch. Programmers do it to create software that solves problems or serves specified purposes, and in the case of Java, it could be versatile platforms ranging from desktop applications, web services, to Android apps.

## How To:

Below is a simple step-by-step guide to creating a new Java project using IntelliJIDEA.

**Step 1 - Launch IntelliJ IDEA & Create New Project**
```Java
File > New > Project
```
**Step 2 - Choose Project Type**
```Java
Select 'Java' on the left; Make sure the correct SDK is selected.
```
**Step 3 - Configure Project**
```Java
Give your project a Name, select a Location, and make sure the 'Create Project from template' box is unchecked.
Click on 'Finish' to create the project.
```
**Step 4 - Create a New Java Class**
```Java
Right-click on 'src' > New > Java Class
Name your class, and click 'OK'
```
**Step 5 - Write HelloWorld Program**
```Java
public class Main {
    public static void main(String[] args) {
        System.out.println("Hello, World");
    }
}
```
When you run this, your output should be:
```Java
Hello, World
```
These steps initialize a project structure, which gives us a platform to build and run our code.

## Deep Dive

Starting a new project wasn't always this straightforward. Back in the day with just text editors and command line tools, organizing your sources and binaries was manually done.

Alternatives to starting a new project in IntelliJ IDEA can be found in other integrated development environments (IDEs) like Eclipse and NetBeans. Each platform generally involves similar steps: Creating new project, selecting the project type, specifying project details, and setting up your working environment.

Under the covers, when you start a new project in Java, the IDE sets up a file structure that separates source files (*.java) from compiled class files (*.class), and resources. It creates a .idea folder and a .iml file for storing project specific settings.

## See Also

There's a mountain of resources out there for new Java coders. Here are links to some excellent ones:
- Oracle Java Documentation [link](https://docs.oracle.com/javase/tutorial/)
- JetBrains IntelliJ IDEA Documentation [link](https://www.jetbrains.com/help/idea/)
- Eclipse Foundation's Java IDE (Eclipse) Tutorial [link](https://www.vogella.com/tutorials/Eclipse/article.html)