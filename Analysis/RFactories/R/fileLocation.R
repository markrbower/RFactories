fileProvider <- function(...) {
  # I want one or more _Factories_ to make inputs to one or more _Builders_
  # that will generate one or more _State_ objects to be used as inputs to fxns.
  
  # What are some things that you would need to locate files?
  # path
  # regular expression (subject? date? channel number? file extension? )
  # You might need several things and those needs may change.
  #
  # The available inputs may change, but the output must be the same:
  #    a valid "fileLocation" object that can return data files.
  # That last part is the important part! I don't have to complete some
  # template with this object; it just needs to be able to returrn
  # a list of data files to me.
  # That is an advantage of objects over "laundry lists"; i.e., as long as
  # this object can provide me with the list of files I need, then that
  # is all I care about!
  #
  # Perhaps it is better to think of the returned object as a "fileProvider";
  # you ask for "hasNext" and "next" and it provides.
  args <- list(...)
  

  
}

