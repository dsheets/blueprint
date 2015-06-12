# blueprint

**blueprint** is an XML (or polyglot HTML) template system. It's a
  command and a MirageOS-compatible OCaml library.

To use **blueprint** from the command line, just

```
$ blue [templates]
```

and the templates will be composed with the result sent to stdout.

## Template language

The blueprint template language is an XML vocabulary that lets you place
'holes' in XML (or polyglot HTML) documents. The `blue` command line
tool automatically binds the blueprint namespace to the `t:`
prefix. Blueprint has 3 tags: *seq*, *insert*, and *let*.

### `t:seq`

`t:seq` is used to group elements together. It is most useful at the
root of a document in order to describe a sequence of elements or to
declare document-level bindings.

### `t:insert`

`t:insert` has a single required attribute, `name`, which gives a name
to the 'hole' that it creates. Bindings against this name will fill this
hole. When holes are filled, the template which fills the hole will be
recursively populated with any bindings in scope. If an expansion step
would use a binding previously used in the expansion, the expansion
stops before the previous hole is filled.

If content is supplied to a `t:insert` element, it will be used as the
default value of the hole in the case that no binding matching `@name`
exists in scope.

Names may only contain alphanumeric characters, '-', and '_'.

### `t:let`

`t:let` has a single required attribute, `name`, which gives a name to
the binding that it creates. Bindings are scoped from the point of
declaration until the end of the parent element. If the parent element
is `t:seq`, the binding will be exported as well. Whitespace before a
`t:let` is elided up to one or two consecutive new lines.

## `blue`

The `blue` tool is a simple command line wrapper around the `Blueprint`
library. `blue` accepts a sequence of files and composes them into a
single output document. The last file listed is used as the template and
all preceding files are used only for their bindings. Any unbound content
in the preceding files is discarded. The result must not contain any
template elements and will be sent to `stdout`. To read from `stdin`,
use `-` as a file name. The `-i` option will disable incomplete template
errors and allow output of partially fulfilled templates.
